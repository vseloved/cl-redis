;;; CL-REDIS implementation of the wire protocol
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :redis)

(defvar *connection* nil "The current Redis connection.")

(defclass redis-connection ()
  ((host
    :initarg  :host
    :initform #(127 0 0 1)
    :reader   connection-host)
   (port
    :initarg  :port
    :initform 6379
    :reader   connection-port)
   (encoding
    :initarg  :encoding
    :initform :utf-8
    :reader   connection-encoding)
   (stream
    :initform nil
    :accessor connection-stream))
  (:documentation "Representation of a Redis connection."))

(defmethod connection-external-format ((connection redis-connection))
  (make-external-format (connection-encoding connection) :eol-style :crlf))

(defmethod initialize-instance :after ((connection redis-connection) &key)
  (open-connection connection))
  
(defun open-connection (connection)
  "Connect a connection object."
  (handler-case
      (setf (connection-stream connection)
            (make-flexi-stream
             (socket-stream
              (socket-connect (connection-host connection)
                              (connection-port connection)
                              :element-type 'octet))
             :external-format (connection-external-format connection)
             :element-type 'octet))
    ((or socket-error stream-error) (e)
      (restart-case (error (wrap-connection-error e))
        (:reconnect ()
          :report "Try to reconnect."
          (open-connection connection))))))

(defun open-connection-p (connection)
  "Is the given connection object connected?"
  (let ((stream (connection-stream connection)))
    (and stream (open-stream-p stream))))

(defun close-connection (connection)
  "Disconnect a connection object."
  (when (open-connection-p connection)
    (close (connection-stream connection))))

(defun ensure-connection (connection)
  "Used to make sure that a connection object is connected before
doing anything with it."
  (unless connection
    (error "No Redis connection specified."))
  (unless (open-connection-p connection)
    (restart-case (error 'redis-connection-error
                         :message "Connection to Redis server lost.")
      (:reconnect ()
        :report "Try to reconnect."
        (open-connection connection)))))

(defmacro with-reconnect-restart (connection &body body)
  "When, inside BODY, an error occurs that breaks the connection stream,
a condition of type REDIS-CONNECTION-ERROR is raised offering
a :reconnect restart."
  (with-unique-names (=connection= =body=)
    `(let ((,=connection= ,connection))
       (ensure-connection ,=connection=)
       (labels ((,=body= ()
                  (handler-case (progn ,@body)
                    ((or socket-error stream-error) (e)
                      (restart-case (error (wrap-connection-error e))
                        (:reconnect ()
                          :report "Try to reconnect."
                          (open-connection ,=connection=)
                          (,=body=)))))))
         (,=body=)))))

(defmacro with-connection ((&key (host #(127 0 0 1))
                                 (port 6379)
                                 (encoding :utf-8))
                           &body body)
  "Evaluate BODY with the current connection bound to a new connection
specified by HOST, PORT, and ENCODING arguments."
  `(let ((*connection* (make-instance 'redis-connection
                                      :host ,host
                                      :port ,port
                                      :encoding ,encoding)))
     (unwind-protect (progn ,@body)
       (close-connection *connection*))))

(defun connected-p ()
  "Is the current connection to Redis server still open?"
  (and *connection* (open-connection-p *connection*)))

(defun connect (&key (host #(127 0 0 1)) (port 6379) (encoding :utf-8))
  "Connect to Redis server."
  (when (connected-p)
    (restart-case (error "A connection to Redis server is already established.")
      (:leave   ()
        :report "Leave it."
        (return-from connect))
      (:replace ()
        :report "Replace it with a new connection."
        (disconnect))))
  (setf *connection* (make-instance 'redis-connection
                                    :host host
                                    :port port
                                    :encoding encoding)))

(defun disconnect ()
  "Disconnect from Redis server."
  (when (connected-p)
    (close-connection *connection*))
  (setf *connection* nil))

(defvar *echo-p* nil
  "Whether the server-client communication should be echoed to the
stream specified by *ECHO-STREAM*.  The default is NIL, meaning no
echoing.")

(defvar *echo-stream* *standard-output*
  "A stream to which the server-client communication will be echoed
for debugging purposes.  The default is *standard-output*.")

;; utils

(defun byte-length (string)
  (octet-length string :external-format (connection-external-format *connection*)))

(defun write-redis-line (fmt &rest args)
  (let ((string (apply #'format nil fmt args)))
    (when *echo-p* (format *echo-stream* "C: ~A~%" string))
    (write-line string (connection-stream *connection*))))

;; conditions

(define-condition redis-error (error)
  ((message :initarg :message :reader redis-error-message))
  (:report (lambda (e stream)
             (format stream "Redis error: ~A" (redis-error-message e))))
  (:documentation "This is the condition type that will be used to
signal virtually all Redis-related errors."))

(define-condition redis-connection-error (redis-error)
  ()
  (:documentation "Conditions of this type are signaled when errors
occur that break the connection stream.  They offer a :reconnect
restart."))

(define-condition redis-error-reply (redis-error)
  ()
  (:documentation "Raised when an error reply is received from Redis
server."))

(define-condition redis-bad-reply (redis-error)
  ()
  (:documentation "Raised when a Redis protocol error is detected."))

(defun wrap-connection-error (e)
  (make-instance 'redis-connection-error :message (princ-to-string e)))

;; outgoing

(defgeneric tell (type cmd &rest args)
  (:documentation
   "Send command to the server through a socket connection, that might be down.
<_:arg TYPE /> is for now :inline, :bulk, or :multi (otherwise error will be signalled).
<_:arg CMD /> is the command string, <_:arg ARGS /> are arguments. (Keyword ~
arguments are also supported).
In case of :bulk command last argument should be a <_:type sequence />."))

(defmethod tell :after (type cmd &rest args)
  (declare (ignore type cmd args))
  (force-output (connection-stream *connection*)))

(defmethod tell (type cmd &rest args)
  (declare (ignore cmd args))
  (error "Type ~A commands are not supported." type))

(defmethod tell ((type (eql :bulk)) cmd &rest args)
  (multiple-value-bind (args bulk) (butlast2 args)
    (assert (and bulk (typep bulk 'string))
            (bulk)
            "Bulk Redis data must be a string")
    (write-redis-line "~A~{ ~A~} ~A" cmd args (byte-length bulk))
    (write-redis-line "~A" bulk)))

(defmethod tell ((type (eql :inline)) cmd &rest args)
  (dolist (arg args)
    (assert (not (blankp arg)) () "Redis argument must be non-empty"))
  (write-redis-line "~A~{ ~A~}" cmd args))

(defmethod tell ((type (eql :inline)) (cmd (eql 'SORT)) &rest args)
  (destructuring-bind (key . options) args
    (assert (not (blankp key)) () "Redis argument must be non-empty")
    (let ((by    (getf options :by))
          (get   (mklist (getf options :get)))
          (desc  (getf options :desc))
          (alpha (getf options :alpha))
          (start (getf options :start))
          (count (getf options :count)))
      (assert (or (and start count)
                  (and (null start) (null count)))
              ()
              "START and END must be either both NIL or both non-NIL.")
      (write-redis-line "SORT ~a~:[~; BY ~:*~a~]~{ GET ~a~}~:[~; DESC~]~:[~; ALPHA~]~:[~; LIMIT ~:*~a ~a~]"
                        key by get desc alpha start count))))

(defmethod tell ((type (eql :multi)) cmd &rest args)
  (let ((bulks (cons (string cmd) args)))
    (write-redis-line "*~A" (length bulks))
    (dolist (bulk bulks)
      (write-redis-line "$~A" (byte-length bulk))
      (write-redis-line "~A" bulk))))

;; ingoing

(defgeneric expect (type)
  (:documentation "Process output of the specified <_:arg type /> from the ~
Redis server"))

(eval-always
  (defmacro def-expect-method (type &body body)
    "Define a specialized EXPECT method"
    (with-unique-names (line char)
      `(defmethod expect ((type (eql ,type)))
         ,(format nil "Process output of type ~a.
A variable REPLY is bound to the output, recieved from the socket."
                  type)
         (let* ((,line (read-line (connection-stream *connection*)))
                (,char (char ,line 0))
                (reply (subseq ,line 1)))
           (when *echo-p* (format *echo-stream* "S: ~A~%" ,line))
           (case ,char
             ((#\-)
              (error 'redis-error-reply :message reply))
             ((#\+ #\: #\$ #\*)
              (progn ,@body))
             (otherwise
              (error 'redis-bad-reply
                     :message (format nil
                                      "Received ~C as an initial reply byte." ,char)))))))))

(def-expect-method :ok
    (assert (string= reply "OK"))
  reply)

(def-expect-method :pong
    (assert (string= reply "PONG"))
  reply)

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
    (if (= size -1)
        nil
        (let ((octets (make-array size :element-type 'octet))
              (stream (connection-stream *connection*)))
          (read-sequence octets stream)
          (read-byte stream)    ; #\Return
          (read-byte stream)    ; #\Linefeed
          (let ((string (octets-to-string octets
                                          :external-format (connection-external-format *connection*))))
            (when *echo-p* (format *echo-stream* "S: ~A~%" string))
            string))))) 

(def-expect-method :multi
  (let ((n (parse-integer reply)))
    (if (= n -1)
        nil
        (loop repeat n collect (expect :bulk)))))

(defmethod expect ((type (eql :end)))
  ;; do nothing
  )

(defmethod expect ((type (eql :list)))
  (cl-ppcre:split " " (expect :bulk)))
    
;; command definition

(defparameter *cmd-prefix* 'red
  "Prefix for functions, implementing Redis commands")

(defmacro def-cmd (cmd (&rest args) docstring cmd-type reply-type)
  "Define and <_:fun export /> a function for processing the Redis ~
command <_:arg cmd /> with the name <_:var *cmd-redix* />-<_:arg cmd />.
<_:arg Cmd-type /> and <_:arg reply-type /> are the types of ~
respectedly underlying <_:fun tell /> and <_:fun expect />, ~
<_: args /> is the <_:fun tell /> arguments, <_:arg docstring /> ~
is the function's doc itself."
  (let ((cmd-name (intern (format nil "~a-~a" *cmd-prefix* cmd))))
    `(progn
       (defun ,cmd-name (,@args)
         ,docstring
         (with-reconnect-restart *connection*
           ,(if-it (position '&rest args)
                   `(apply #'tell ,cmd-type
                           ',cmd
                           ,@(subseq args 0 it)                   
                           ,(nth (1+ it) args))
                   `(tell ,cmd-type ',cmd ,@args))
           (expect ,reply-type)))
       (export ',cmd-name :redis))))

;;; end