;;; CL-REDIS implementation of the wire protocol
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :redis)


;; utils

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
    (write-line string redis-out)
    (finish-output redis-out)))


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

(defgeneric tell (type cmd &rest args)
  (:documentation "Send a command to Redis server over a socket connection.
TYPE must be one of the keywords :INLINE, :BULK, or :MULTI.  Otherwise an ~
error will be signalled.  CMD is the command name (a string or a symbol), ~
and ARGS are its arguments (keyword arguments are also supported).  In the ~
case of a :BULK command the last argument must be a string."))

(defmethod tell :around (type cmd &rest args)
  (declare (ignore type args))
  (let ((cmd (ppcre:split "-" (string cmd))))
    (call-next-method)))

(defmethod tell :after (type cmd &rest args)
  (declare (ignore type cmd args))
  (force-output (connection-socket *connection*)))

(defmethod tell (type cmd &rest args)
  (declare (ignore cmd args))
  (error "Commands of type ~A are not supported." type))

(defmethod tell ((type (eql :generic)) cmd &rest args)
  (format-redis-line "*~A" (1+ (length args)))
  (mapcar #`((format-redis-line "$~A" (length _))
             (format-redis-line "~A" _))
          (cons (string cmd) args)))

(defmethod tell ((type (eql :inline)) cmd &rest args)
  (format-redis-line "~A~{ ~A~}" cmd args))

(defmethod tell ((type (eql :bulk)) cmd &rest args)
  (multiple-value-bind (args bulk) (butlast2 args)
    (check-type bulk string)
    (format-redis-line "~A~{ ~A~} ~A" cmd args (byte-length bulk))
    (format-redis-line "~A" bulk)))

(defmethod tell ((type (eql :multi)) cmd &rest args)
  (let ((bulks (cons (string cmd) args)))
    (format-redis-line "*~A" (length bulks))
    (dolist (bulk bulks)
      (format-redis-line "$~A" (byte-length bulk))
      (format-redis-line "~A" bulk))))

;; command-specific TELL methods

(defmethod tell ((type (eql :inline)) (cmd (eql 'SORT)) &rest args)
  (flet ((send-request (key &key by get desc alpha start end)
           (assert (or (and start end)
                       (and (null start) (null end))))
           (format-redis-line "SORT ~a~:[~; BY ~:*~a~]~{ GET ~a~}~:[~; DESC~]~
~:[~; ALPHA~]~:[~; LIMIT ~:*~a ~a~]"
                             key by (mklist get) desc alpha start end)))
    (apply #'send-request args)))

(macrolet ((z...range-body ()
             `(flet ((send-request (key start end &rest args &key withscores)
                       (format-redis-line "~a ~a ~a ~a~:[~; WITHSCORES~]"
                                         cmd key start end withscores)))
                (apply #'send-request args))))
  (defmethod tell ((type (eql :inline)) (cmd (eql 'ZRANGE)) &rest args)
    (z...range-body))
  (defmethod tell ((type (eql :inline)) (cmd (eql 'ZREVRANGE)) &rest args)
    (z...range-body)))

(macrolet ((z...store-body ()
             `(flet ((send-request (dstkey n keys
                                           &rest args &key weights aggregate)
                       (assert (integerp n))
                       (assert (= n (length keys)))
                       (when weights
                         (assert (= (length keys) (length weights)))
                         (assert (every #'numberp weights)))
                       (when aggregate
                         (assert (member aggregate '(:sum :min :max))))
                       (format-redis-line "~a ~a ~a~:[~; ~:*~{~a ~}~]~
~:[~;WEIGHTS ~:*~{~a ~}~]~:[~;AGGREGATE ~:*~a~]"
                                         cmd dstkey n keys weights aggregate)))
                (apply #'send-request args))))
  (defmethod tell ((type (eql :inline)) (cmd (eql 'ZUNIONSTORE)) &rest args)
    (z...store-body))
  (defmethod tell ((type (eql :inline)) (cmd (eql 'ZINTERSTORE)) &rest args)
    (z...store-body)))


;; receiving replies 

(defgeneric expect (type)
  (:documentation "Receive and process the reply of the given type
from Redis server."))

(eval-always
  (defmacro with-redis-in ((line char) &body body)
    `(let* ((,line (read-line (connection-socket *connection*)))
            (,char (char ,line 0)))
       (when *echo-p* (format *echo-stream* "<  ~A~%" ,line))
       ,@body))

  (defmacro def-expect-method (type &body body)
    "Define a specialized EXPECT method.  BODY may refer to the ~
variable REPLY, which is bound to the reply received from Redis ~
server with the first character removed."
    (with-unique-names (line char)
      `(defmethod expect ((type (eql ,type)))
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
                                                      ,char)))))))))))

(defmethod expect ((type (eql :anything)))
  "Receive and process status reply, which is just a string, preceeded with +."
  (case (peek-char nil (connection-socket *connection*))
    (#\+ (expect :status))
    (#\: (expect :inline))
    (#\$ (expect :bulk))
    (#\* (expect :multi))
    (otherwise (expect :status))))  ; will do error-signalling

(defmethod expect ((type (eql :status)))
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
                    (read-byte socket)  ; #\Return
                    (read-byte socket)  ; #\Linefeed
                    (let ((string (babel:octets-to-string octets
                                                          :encoding :utf-8)))
                      (when *echo-p* (format *echo-stream* "<  ~A~%" string))
                      (if (string= string "nil")
                          nil
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
         :collect (expect :bulk)))))

(def-expect-method :queued
  (let ((n (parse-integer reply)))
    (unless (= n -1)
      (loop :repeat n
         :collect (expect :anything)))))

(defmethod expect ((type (eql :pubsub)))
  (let ((in (connection-socket *connection*)))
    (loop :collect (with-redis-in (line char)
                     (list (expect :bulk)
                           (expect :bulk)
                           (expect :inline)))
       :do (let ((next-char (read-char-no-hang in)))
             (if next-char (progn (unread-char next-char in)
                                  ;; after unread-char #\Newline is
                                  ;; received from iolib socket!
                                  (read-char in))
                 (loop-finish))))))

(defmethod expect ((type (eql :end)))
  ;; Used for commands QUIT and SHUTDOWN (does nothing)
  )

(defmethod expect ((type (eql :list)))
  ;; Used to make Redis KEYS command return a list of strings (keys)
  ;; rather than a single string
  (cl-ppcre:split " " (expect :bulk)))

    
;; high-level command definition

(defparameter *cmd-prefix* 'red
  "Prefix for functions names that implement Redis commands.")

(defmacro def-cmd (cmd (&rest args) docstring cmd-type reply-type)
  "Define and export a function with the name <*CMD-REDIX*>-<CMD> for ~
processing a Redis command CMD.  Here CMD-TYPE and REPLY-TYPE are the ~
command and reply types respectively, ARGS are the command arguments, ~
and DOCSTRING is the command documentation string."
  (let ((cmd-name (intern (format nil "~a-~a" *cmd-prefix* cmd))))
    `(progn
       (defun ,cmd-name ,args
         ,docstring
         (with-reconnect-restart *connection*
           ,(if-it (position '&rest args)
                   `(apply #'tell ,cmd-type ',cmd
                           ,@(subseq args 0 it)
                           ,(nth (1+ it) args))
                   `(tell ,cmd-type ',cmd ,@args))
           (expect ,reply-type)))
       (export ',cmd-name :redis))))

;; pipelining

(defmacro with-pipelining (&body body)
  "Delay execution of EXPECT's inside BODY to the end, so that all
commands are first sent to the server and then their output is received
and collected into a list.  So commands return :PIPELINED instead of the
expected results."
  (with-gensyms (old-expect pipeline)
    `(let ((,old-expect (fdefinition 'expect))
           ,pipeline)
       (unwind-protect
            (progn
              (setf (fdefinition 'expect) (lambda (&rest args)
                                            (push args ,pipeline)
                                            :pipelined))
              ,@body)
         (setf (fdefinition 'expect) ,old-expect))
       (mapcar #`(apply #'expect _) (nreverse ,pipeline)))))

;;; end