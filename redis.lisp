;;; CL-REDIS implementation of the wire protocol
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :redis)


;; utils
(declaim (optimize (safety 3) (speed 0) (space 0) (debug 3)))
(defun byte-length (string)
  "Return the length of STRING if encoded using utf-8 external format."
  (length (babel:string-to-octets string)))

(defun format-redis-line (fmt &rest args)
  "Write a CRLF-terminated string formatted according to the given control ~
string FMT and its arguments ARGS to the stream of the current connection.
If *ECHOP-P* is not NIL, write that string to *ECHO-STREAM*, too."
  (let ((string (apply #'format nil fmt args))
        (redis-out (connection-socket *connection*)))
    (when *echo-p* (format *echo-stream* " > ~A~%" string))
    (write-sequence (babel:string-to-octets string
                                            :encoding :utf-8)
                    redis-out)
    (terpri redis-out)))


;; conditions

(define-condition redis-error (error)
  ((message
    :initarg  :message
    :reader   redis-error-message)
   (comment
    :initform nil
    :initarg  :comment
    :reader   redis-error-comment))
  (:report (lambda (e stream)
             (format stream
                     "Redis error: ~A~:[~;~2&~:*~A~]"
                     (redis-error-message e)
                     (redis-error-comment e))))
  (:documentation "This is the condition type that will be used to ~
signal virtually all Redis-related errors."))

(define-condition redis-connection-error (redis-error)
  ()
  (:documentation "Conditions of this type are signaled when errors ~
occur that break the connection stream.  They offer a :reconnect ~
restart."))

(define-condition redis-error-reply (redis-error)
  ()
  (:documentation "Raised when an error reply is received from Redis ~
server."))

(define-condition redis-bad-reply (redis-error)
  ()
  (:documentation "Raised when a Redis protocol error is detected."))


;; sending commands to the server

(defgeneric tell (cmd &rest args)
  (:documentation "Send a command to Redis server over a socket connection.
CMD is the command name (a string or a symbol), and ARGS are its arguments
\(keyword arguments are also supported)."))

(defmethod tell :after (cmd &rest args)
  (declare (ignore cmd args))
  (force-output (connection-socket *connection*)))

(defmethod tell (cmd &rest args)
  (format-redis-line "*~A" (1+ (length args)))
  (mapcar (lambda (arg)
            (let ((arg (princ-to-string arg)))
              (format-redis-line "$~A" (byte-length arg))
              (format-redis-line "~A"  arg)))
          (cons cmd args)))

(defmethod tell ((cmd (eql 'SORT)) &rest args)
  (flet ((send-request (key &key by get desc alpha start end)
           (assert (or (and start end)
                       (and (null start) (null end))))
           (apply #'tell "SORT"
                  (nconc (list key)
                         (when by (list "BY" by))
                         (when get (list "GET" get))
                         (when desc (list "DESC"))
                         (when alpha (list "ALPHA"))
                         (when start (list "LIMIT" start end))))))
    (apply #'send-request args)))

(flet ((send-request (cmd key start end &key withscores)
         (apply #'tell (princ-to-string cmd)
                (nconc (list key start end)
                       (when withscores (list "WITHSCORES"))))))
  (defmethod tell ((cmd (eql 'ZRANGE)) &rest args)
    (apply #'send-request cmd args))
  (defmethod tell ((cmd (eql 'ZREVRANGE)) &rest args)
    (apply #'send-request cmd args)))

(flet ((send-request (cmd dstkey n keys &key weights aggregate)
         (assert (integerp n))
         (assert (= n (length keys)))
         (when weights
           (assert (= (length keys) (length weights)))
           (assert (every #'numberp weights)))
         (when aggregate
           (assert (member aggregate '(:sum :min :max))))
         (apply #'tell (princ-to-string cmd)
                (nconc (list dstkey n)
                       keys
                       (when weights (cons "WEIGHTS" weights))
                       (when aggregate (list "AGGREGATE" aggregate))))))
  (defmethod tell ((cmd (eql 'ZUNIONSTORE)) &rest args)
    (apply #'send-request cmd args))
  (defmethod tell ((cmd (eql 'ZINTERSTORE)) &rest args)
    (apply #'send-request cmd args)))


;; receiving replies





(defgeneric %expect (type)
  (:documentation "Receive and process the reply of the given type
from Redis server."))

(eval-always
  (defmacro with-redis-in ((line char) &body body)
    `(let ((,line (read-line (connection-socket *connection*))))
       (when (> (length ,line) 0)
	 (let ((,char (char ,line 0)))
	   (when *echo-p* (format *echo-stream* "<  ~A~%" ,line))
	   ,@body))))

  (defmacro def-expect-method (type &body body)
    "Define a specialized EXPECT method.  BODY may refer to the ~
variable REPLY, which is bound to the reply received from Redis ~
server with the first character removed."
    (with-unique-names (line char)
      `(defmethod %expect ((type (eql ,type)))
         ,(format nil "Receive and process the reply of type ~a."
                  type)
         (with-redis-in (,line ,char)
           (let ((reply (subseq ,line 1)))
             (if (string= ,line "+QUEUED") "QUEUED"
		 (case ,char
		   (#\- (error 'redis-error-reply :message reply))
		   ((#\+ #\: #\$ #\*) ,@body)
		   (otherwise (error 'redis-bad-reply
				     :message (format nil "Received ~C as the ~
initial reply byte."
						      ,char))))
		 )))))))

(defmethod %expect ((type (eql :anything)))
  "Receive and process status reply, which is just a string, preceeded with +."
  (case (peek-char nil (connection-socket *connection*))
    (#\+ (%expect :status))
    (#\: (%expect :inline))
    (#\$ (%expect :bulk))
    (#\* (%expect :multi))
    (otherwise (%expect :status))))  ; will do error-signalling

(defmethod %expect ((type (eql :status)))
  "Receive and process status reply, which is just a string, preceeded with +."
  (with-redis-in (line char)
    (case char
      (#\- (error 'redis-error-reply :message (subseq line 1)))
      (#\+ (subseq line 1))
      (otherwise (error 'redis-bad-reply
                        :message (format nil "Received ~C as the initial reply ~
byte."
                                         char))))))

(def-expect-method :inline
  reply)

(def-expect-method :boolean
  (ecase (char reply 0)
    (#\0 nil)
    (#\1 t)))

(def-expect-method :integer
  (values (parse-integer reply)))

(macrolet ((read-bulk-reply (&optional reply-transform)
             `(let ((n (parse-integer reply)))
                (unless (<= n 0)
		  (let ((octets (make-array n :element-type '(unsigned-byte 8)))
				 (socket (connection-socket *connection*)))
			     (read-sequence octets socket)
			     (read-byte socket)	; #\Return
			     (read-byte socket)	; #\Linefeed
			     (let ((string (babel:octets-to-string octets
								   :encoding :utf-8)))
			       (when *echo-p* (format *echo-stream* "<  ~A~%" string))
			       (if (string= string "nil") nil
				   (if ,reply-transform (funcall ,reply-transform string)
				       string))))))))
  (def-expect-method :bulk
    (read-bulk-reply))
  (def-expect-method :float
    (read-bulk-reply (lambda (x) (parse-float x :type 'double-float)))))

(def-expect-method :multi
  (let ((n (parse-integer reply)))
    (unless (= n -1)
      (loop :repeat n
	 :collect   (%expect :bulk)))))

(def-expect-method :queued
  (let ((n (parse-integer reply)))
    (unless (= n -1)
      (loop :repeat n
         :collect (%expect :anything)))))

(defmethod %expect ((type (eql :pubsub)))
  (let ((in (connection-socket *connection*)))
    (loop :collect (with-redis-in (line char)
                     (list (%expect :bulk)
                           (%expect :bulk)
                           (%expect :inline)))
       :do (let ((next-char (read-char-no-hang in)))
             (if next-char (progn (unread-char next-char in)
                                  ;; after unread-char #\Newline is
                                  ;; received from iolib socket!
                                  (read-char in))
                 (loop-finish))))))

(defmethod %expect ((type (eql :end)))
  ;; Used for commands QUIT and SHUTDOWN (does nothing)
  )

(defmethod %expect ((type (eql :list)))
  ;; Used to make Redis KEYS command return a list of strings (keys)
  ;; rather than a single string
  (cl-ppcre:split " " (%expect :bulk)))

(defun flush-connection ()
  (let ((in (connection-socket *connection*)))
    (clear-input in)))

(defvar *pipeline* nil)
(defvar *pipelining?* nil)


(defun expect (type)
  (if *pipelining?*
      (funcall (lambda (&rest args)
		 (push args *pipeline*)
		 :pipelined) type)
      (prog1 (%expect type)
	     (flush-connection))))

;; high-level command definition

(defparameter *cmd-prefix* 'red
  "Prefix for functions names that implement Redis commands.")

(defmacro def-cmd (cmd (&rest args) reply-type docstring &optional (pre-condition nil))
  "Define and export a function with the name <*CMD-REDIX*>-<CMD> for ~
processing a Redis command CMD.  Here REPLY-TYPE is the expected reply ~
format."
  (let ((cmd-name (intern (format nil "~a-~a" *cmd-prefix* cmd))))
    `( ,@(if pre-condition (list 'when pre-condition) (list 'progn))
	 (defun ,cmd-name ,args
	   ,docstring
	   (return-from ,cmd-name
	     (with-reconnect-restart *connection*
	       ,(if-it (position '&rest args)
		       `(apply #'tell ',cmd
			       ,@(subseq args 0 it)
			       ,(nth (1+ it) args))
		       `(tell ',cmd ,@args))
               (expect ,reply-type))))
	 (export ',cmd-name :redis))))

;; pipelining

(defmacro with-pipelining (&body body)
  "Delay execution of EXPECT's inside BODY to the end, so that all
commands are first sent to the server and then their output is received
and collected into a list.  So commands return :PIPELINED instead of the
expected results."
    `(let (redis::*pipeline*)
       (let ((*pipelining?* t))
	 ,@body)
       (unwind-protect (mapcar (lambda (args) (apply #'%expect args))
			       (nreverse redis::*pipeline*))
	 (flush-connection))))

;;; end