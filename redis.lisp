;;; CL-REDIS implementation of the wire protocol
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :redis)


;; utils

(defun byte-length (string)
  "Return the length of STRING if encoded using the external format ~
of the current connection."
  (octet-length string
                :external-format (connection-external-format *connection*)))

(defun write-redis-line (fmt &rest args)
  "Write a CRLF-terminated string formatted according to the given control ~
string FMT and its arguments ARGS to the stream of the current connection.
If *ECHOP-P* is not NIL, write that string to *ECHO-STREAM*, too."
  (let ((string (apply #'format nil fmt args)))
    (when *echo-p* (format *echo-stream* "C: ~A~%" string))
    (write-line string (connection-stream *connection*))))


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
  (force-output (connection-stream *connection*)))

(defmethod tell (type cmd &rest args)
  (declare (ignore cmd args))
  (error "Commands of type ~A are not supported." type))

(defmethod tell ((type (eql :inline)) cmd &rest args)
  (write-redis-line "~A~{ ~A~}" cmd args))

(defmethod tell ((type (eql :bulk)) cmd &rest args)
  (multiple-value-bind (args bulk) (butlast2 args)
    (check-type bulk string)
    (write-redis-line "~A~{ ~A~} ~A" cmd args (byte-length bulk))
    (write-redis-line "~A" bulk)))

(defmethod tell ((type (eql :multi)) cmd &rest args)
  (let ((bulks (cons (string cmd) args)))
    (write-redis-line "*~A" (length bulks))
    (dolist (bulk bulks)
      (write-redis-line "$~A" (byte-length bulk))
      (write-redis-line "~A" bulk))))

;; command-specific TELL methods

(defmethod tell ((type (eql :inline)) (cmd (eql 'SORT)) &rest args)
  (flet ((send-request (key &key by get desc alpha start end)
           (unless (or (and start end)
                       (and (null start) (null end)))
             (error "START and COUNT must be either both NIL or both non-NIL."))
           (write-redis-line "SORT ~a~:[~; BY ~:*~a~]~{ GET ~a~}~:[~; DESC~]~:[~; ALPHA~]~:[~; LIMIT ~:*~a ~a~]"
                             key by (mklist get) desc alpha start end)))
    (apply #'send-request args)))

;; receiving replies 

(defgeneric expect (type)
  (:documentation "Receive and process the reply of the given type
from Redis server."))

(eval-always
  (defmacro def-expect-method (type &body body)
    "Define a specialized EXPECT method.  BODY may refer to the ~
variable REPLY, which is bound to the reply received from Redis ~
server with the first character removed."
    (with-unique-names (line char)
      `(defmethod expect ((type (eql ,type)))
         ,(format nil "Receive and process the reply of type ~a."
                  type)
         (let* ((,line (read-line (connection-stream *connection*)))
                (,char (char ,line 0))
                (reply (subseq ,line 1)))
           (when *echo-p* (format *echo-stream* "S: ~A~%" ,line))
           (if (string= ,line "QUEUED") "QUEUED"
               (case ,char
                 (#\- (error 'redis-error-reply :message reply))
                 ((#\+ #\: #\$ #\*) ,@body)
                 (otherwise (error 'redis-bad-reply
                                   :message (format nil "Received ~C as the ~
initial reply byte."
                                                    ,char))))))))))

(def-expect-method :inline
  reply)

(def-expect-method :boolean
  (ecase (char reply 0)
    (#\0 nil)
    (#\1 t)))

(def-expect-method :integer
  (values (parse-integer reply)))

(def-expect-method :bulk
  (let ((size (parse-integer reply)))
    (unless (= size -1)
      (let ((octets (make-array size :element-type 'octet))
            (stream (connection-stream *connection*)))
        (read-sequence octets stream)
        (read-byte stream)    ; #\Return
        (read-byte stream)    ; #\Linefeed
        (let ((string
               (octets-to-string octets
                                 :external-format
                                 (connection-external-format *connection*))))
          (when *echo-p* (format *echo-stream* "S: ~A~%" string))
          (unless (string= string "nil")  ; account for `special' nil value
            string))))))

(def-expect-method :multi
  (let ((n (parse-integer reply)))
    (unless (= n -1)
      (loop :repeat n
         :collect (expect :bulk)))))

(defmethod expect ((type (eql :status)))
  "Receive and process status reply, which is just a string, preceeded with +."
  (let* ((line (read-line (connection-stream *connection*)))
         (char (char line 0)))
    (when *echo-p* (format *echo-stream* "S: ~A~%" line))
    (case char
      (#\- (error 'redis-error-reply :message reply))
      (#\+ (subseq line 1))
      (otherwise (error 'redis-bad-reply
                        :message (format nil "Received ~C as the initial reply ~
byte."
                                         char))))))

(defmethod expect ((type (eql :end)))
  "Used for commands QUIT and SHUTDOWN."
  ;; do nothing
  )

(defmethod expect ((type (eql :list)))
  "Used to make Redis KEYS command return a list of strings (keys) ~
rather than a single string."
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

;;; end