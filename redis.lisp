;;; CL-REDIS implementation of the wire protocol
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package #:redis)


;; Utils.

(defun format-redis-number (char number)
  "Write a prefix char and a number to the stream of the current connection.
If *ECHOP-P* is not NIL, write that string to *ECHO-STREAM*, too."
  (let* ((out (conn-stream *connection*))
         (soc (flex:flexi-stream-stream out)))
    (when *echo-p* (format *echo-stream* " > ~A~A~%" char number))
    (write-byte (char-code char) soc)
    (princ number out)
    (write-byte 13 soc)
    (write-byte 10 soc)))

(defun format-redis-string (string)
  "Write a string and CRLF-terminator to the stream of the current connection.
If *ECHOP-P* is not NIL, write that string to *ECHO-STREAM*, too."
  (let ((soc (flex:flexi-stream-stream (conn-stream *connection*))))
    (when *echo-p* (format *echo-stream* " > ~A~%" string))
    (write-sequence (babel:string-to-octets string :encoding :UTF-8) soc)
    (write-byte 13 soc)
    (write-byte 10 soc)))

(defun ensure-string (obj)
  (typecase obj
    (string obj)
    (symbol (string obj))
    (t (prin1-to-string obj))))

;;; Conditions

(define-condition redis-error (error)
  ((error :initform nil
          :initarg :error
          :reader redis-error-error)
   (message :initform nil
            :initarg :message
            :reader redis-error-message))
  (:report (lambda (e stream)
             (format stream
                     "Redis error: ~A~:[~;~2&~:*~A~]"
                     (redis-error-error e)
                     (redis-error-message e))))
  (:documentation "Any Redis-related error."))

(define-condition redis-connection-error (redis-error)
  ()
  (:documentation "Conditions of this type are signaled when errors occur
that break the connection stream.  They offer a :RECONNECT restart."))

(define-condition redis-error-reply (redis-error)
  ()
  (:documentation "Error reply is received from Redis server."))

(define-condition redis-bad-reply (redis-error)
  ()
  (:documentation "Redis protocol error is detected."))


;;; Sending commands to the server

(defgeneric tell (cmd &rest args)
  (:documentation "Send a command to Redis server over a socket connection.
CMD is the command name (a string or a symbol), and ARGS are its arguments
\(keyword arguments are also supported)."))

(defmethod tell :after (cmd &rest args)
  (declare (ignore cmd args))
  (force-output (conn-stream *connection*)))

(defmethod tell (cmd &rest args)
  (let ((all-args (cl:append (ppcre:split "-" (ensure-string cmd))
                             args)))
    (format-redis-number #\* (length all-args))
    (dolist (arg all-args)
      (let ((arg (ensure-string arg)))
        (format-redis-number #\$ (babel:string-size-in-octets arg :encoding :UTF-8))
        (format-redis-string arg)))))


;; Pipelining

(defvar *pipelined* nil
  "Indicates, that commands are sent in pipelined mode.")

(defvar *pipeline* nil
  "A list of expected results from the current pipeline.")

(defmacro with-pipelining (&body body)
  "Delay execution of EXPECT's inside BODY to the end, so that all
commands are first sent to the server and then their output is received
and collected into a list.  So commands return :PIPELINED instead of the
expected results."
  `(if *pipelined*
       (progn
         (warn "Already in a pipeline.")
         ,@body)
       (with-reconnect-restart
         (let (*pipeline*)
           (let ((*pipelined* t))
             ,@body)
           (mapcar #'expect (reverse *pipeline*))))))


;;; Receiving replies

(defgeneric expect (type &key timeout)
  (:documentation "Receive and process the reply of the given type from Redis server.

If specified, TIMEOUT is the number of seconds to wait for a reply before
returning.

Returns two values, the first is the reply or NIL. The second is a real number
indicating the time remaining in the timeout or NIL if the read timed out/there
was no timeout."))

(defmethod expect :around (type &key timeout)
  (if *pipelined*
      (progn (push type *pipeline*)
             :pipelined)
      ;; Wait for the socket to be ready, up to TIMEOUT seconds
      (let (remaining-timeout)
        (if (or (null timeout)
                (setq remaining-timeout
                      (nth-value 1 (usocket:wait-for-input (conn-socket *connection*)
                                                           :timeout timeout))))
            (call-next-method)
            (values nil remaining-timeout)))))

(eval-always

(defmacro with-redis-in ((line char) &body body)
  `(let* ((,line (read-line (conn-stream *connection*)))
          (,char (char ,line 0)))
     (when *echo-p* (format *echo-stream* "<  ~A~%" ,line))
     ,@body))

(defmacro def-expect-method (type &body body)
  "Define a specialized EXPECT method.  BODY may refer to the ~
variable REPLY, which is bound to the reply received from Redis ~
server with the first character removed."
  (with-unique-names (line char)
    `(defmethod expect ((type (eql ,type)) &key &allow-other-keys)
       ,(fmt "Receive and process the reply of type ~A." type)
       (with-redis-in (,line ,char)
         (let ((reply (subseq ,line 1)))
           (if (string= ,line "+QUEUED") "QUEUED"
               (case ,char
                 (#\- (error 'redis-error-reply :message reply))
                 ((#\+ #\: #\$ #\*) ,@body)
                 (otherwise
                  (error 'redis-bad-reply
                         :message (fmt "Received ~C as the initial reply byte."
                                       ,char))))))))))
) ; end of eval-always

(defmethod expect ((type (eql :anything)) &key &allow-other-keys)
  "Receive and process status reply, which is just a string, preceeded with +."
  (case (peek-char nil (conn-stream *connection*))
    (#\+ (expect :status))
    (#\: (expect :integer))
    (#\$ (expect :bulk))
    (#\* (expect :multi))
    (otherwise (expect :status))))  ; will do error-signalling

(defmethod expect ((type (eql :status)) &key &allow-other-keys)
  "Receive and process status reply, which is just a string, preceeded with +."
  (with-redis-in (line char)
    (case char
      (#\- (error 'redis-error-reply :message (subseq line 1)))
      (#\+ (subseq line 1))
      (otherwise (error 'redis-bad-reply
                        :message (fmt "Received ~C as the initial reply byte."
                                      char))))))

(def-expect-method :inline
  reply)

(def-expect-method :boolean
  (ecase (char reply 0)
    (#\0 nil)
    (#\1 t)))

(def-expect-method :integer
  (values (parse-integer reply)))

(defmacro read-bulk-reply (&key post-processing (decode t))
  (with-gensyms (n bytes in str)
    `(let ((,n (parse-integer reply)))
       (unless (< ,n 0)
         (let ((,bytes (make-array ,n :element-type 'flex:octet))
               (,in (conn-stream *connection*)))
           (read-sequence ,bytes ,in)
           (read-byte ,in)               ; #\Return
           (read-byte ,in)               ; #\Linefeed
           ,(if decode
                `(let ((,str (babel:octets-to-string ,bytes :encoding :UTF-8)))
                   (when *echo-p* (format *echo-stream* "<  ~A~%" ,str))
                   (unless (string= "nil" ,str)
                     (if ,post-processing
                         (funcall ,post-processing ,str)
                         ,str)))
                bytes))))))

(def-expect-method :bulk
  (read-bulk-reply))

(def-expect-method :multi
  (let ((n (parse-integer reply)))
    (unless (= n -1)
      (loop :repeat n
         :collect (ecase (peek-char nil (conn-stream *connection*))
                    (#\: (expect :integer))
                    (#\$ (expect :bulk))
                    (#\* (expect :multi)))))))

(def-expect-method :queued
  (let ((n (parse-integer reply)))
    (unless (= n -1)
      (loop :repeat n
         :collect (expect :anything)))))

(defmethod expect ((type (eql :pubsub)) &key &allow-other-keys)
  (let ((in (conn-stream *connection*)))
    (loop :collect (with-redis-in (line char)
                     (list (expect :bulk)
                           (expect :bulk)
                           (expect :inline)))
       :do (let ((next-char (read-char-no-hang in)))
             (if next-char (unread-char next-char in)
                 (loop-finish))))))

(defmethod expect ((type (eql :end)) &key &allow-other-keys)
  ;; Used for commands QUIT and SHUTDOWN (does nothing)
  )

(defmethod expect ((type (eql :list)) &key &allow-other-keys)
  ;; Used to make Redis KEYS command return a list of strings (keys)
  ;; rather than a single string
  (cl-ppcre:split " " (expect :bulk)))

(def-expect-method :float
  (read-bulk-reply :post-processing (lambda (x)
                                      (parse-float x :type 'double-float))))

(def-expect-method :bytes
  (read-bulk-reply :decode nil))


;;; Command definition

(defparameter *cmd-prefix* 'red
  "Prefix for functions names that implement Redis commands.")

(defmacro def-cmd (cmd (&rest args) reply-type docstring)
  "Define and export a function with the name <*CMD-REDIX*>-<CMD> for
processing a Redis command CMD.  Here REPLY-TYPE is the expected reply
format."
  (let ((cmd-name (intern (fmt "~:@(~A-~A~)" *cmd-prefix* cmd))))
    `(eval-always
       (defun ,cmd ,args
         ,docstring
         (return-from ,cmd
           (with-reconnect-restart
             ,(cond-it
               ((position '&optional args)
                `(apply #'tell ',cmd ,@(subseq args 0 it)
                        (let ((optional-args (list ,@(nthcdr (1+ it) args))))
                          (subseq optional-args 0 (position nil optional-args)))))
               ((position '&rest args)
                `(apply #'tell ',cmd ,@(subseq args 0 it) ,(nth (1+ it) args)))
               (t `(tell ',cmd ,@args)))
             (prog1 (expect ,reply-type)
               (unless *pipelined*
                 (clear-input (conn-stream *connection*)))))))
       (abbr ,cmd-name ,cmd)
       (export ',cmd-name '#:redis)
       (import ',cmd '#:red)
       (export ',cmd '#:red))))

;;; end
