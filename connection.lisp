;;; CL-REDIS connection handling
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :redis)

(defvar *connection* nil "The current Redis connection.")


;; debugging

(defvar *echo-p* nil
  "Whether the server-client communication should be echoed to the
stream specified by *ECHO-STREAM*.  The default is NIL, meaning no
echoing.")

(defvar *echo-stream* *standard-output*
  "A stream to which the server-client communication will be echoed
for debugging purposes.  The default is *STANDARD-OUTPUT*.")


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
    :accessor connection-stream)
   (socket
    :initform nil
    :accessor connection-socket))
  (:documentation "Representation of a Redis connection."))

(defmethod connection-external-format ((connection redis-connection))
  "Return the external format of CONNECTION based on its encoding."
  (make-external-format (connection-encoding connection)
                        :eol-style :crlf))

(defmacro signal-connection-error-with-reconnect-restart
    (&key message comment restart)
  "Signal the condition of type REDIS-CONNECTION-ERROR denoted by the given
MESSAGE and COMMENT offering a :reconnect restart given by RESTART."
  `(restart-case (error 'redis-connection-error
                        :message ,message
                        :comment ,comment)
     (:reconnect ()
       :report "Try to reconnect."
       ,restart)))

(defmacro provide-reconnect-restart (expression &body body)
  "When, during the execution of EXPRESSION, an error occurs that can break
the connection stream, a condition of type REDIS-CONNECTION-ERROR is raised
offering a :reconnect restart whose body is given by BODY."
  (with-gensyms (err)
    `(handler-case ,expression
       (connection-refused-error (,err)
         ;; Errors of this type commonly occur when there is no Redis server
         ;; running, or when one tries to connect to the wrong host or port.
         ;; We anticipate this by providing a helpful comment.
         (signal-connection-error-with-reconnect-restart
          :message ,err
          :comment "Make sure Redis server is running and check your connection parameters."
          :restart (progn ,@body)))
       ((or socket-error stream-error) (,err)
         (signal-connection-error-with-reconnect-restart
          :message ,err
          :restart (progn ,@body))))))

(defmethod initialize-instance :after ((connection redis-connection) &key)
  (open-connection connection))
  
(defun open-connection (connection)
  "Create a socket connection to the host and port of CONNECTION and
set the stream of CONNECTION to the associated stream."
  (provide-reconnect-restart
      (setf (connection-socket connection)
            (socket-connect (connection-host connection)
                            (connection-port connection)
                            :element-type 'octet)
            (connection-stream connection)
            (make-flexi-stream (socket-stream (connection-socket connection))
                               :external-format (connection-external-format
                                                 connection)
                               :element-type 'octet))
    (open-connection connection)))

(defun open-connection-p (connection)
  "Is the stream of CONNECTION open?"
  (and-it (connection-stream connection)
          (open-stream-p it)))

(defun close-connection (connection)
  "Close the stream of CONNECTION."
  (socket-close (connection-socket connection)))

(defun reopen-connection (connection)
  "Close and reopen CONNECTION."
  (close-connection connection)
  (open-connection connection))

(defun ensure-connection (connection)
  "Ensure that CONNECTION is open before doing anything with it."
  (unless connection
    (error "No Redis connection specified."))
  (unless (open-connection-p connection)
    (signal-connection-error-with-reconnect-restart
     :message "Connection to Redis server lost."
     :restart (reopen-connection connection))))

(defmacro with-reconnect-restart (connection &body body)
  "When, inside BODY, an error occurs that breaks the stream of CONNECTION,
a condition of type REDIS-CONNECTION-ERROR is raised offering a :reconnect
restart."
  (with-gensyms (=connection= =body=)
    `(let ((,=connection= ,connection))
       (ensure-connection ,=connection=)
       (labels ((,=body= ()
                  (provide-reconnect-restart (progn ,@body)
                    (reopen-connection ,=connection=)
                    (,=body=))))
         (,=body=)))))

(defmacro with-connection ((&key (host #(127 0 0 1))
                                 (port 6379)
                                 (encoding :utf-8))
                           &body body)
  "Evaluate BODY with the current connection bound to a new connection
specified by the given HOST, PORT, and ENCODING."
  `(let ((*connection* (make-instance 'redis-connection
                                      :host ,host
                                      :port ,port
                                      :encoding ,encoding)))
     (unwind-protect (progn ,@body)
       (ignore-errors (close-connection *connection*)))))

(defmacro with-recursive-connection ((&key (host #(127 0 0 1))
                                           (port 6379)
                                           (encoding :utf-8))
                                     &body body)
  "Execute BODY with *CONNECTION* bound to the default Redis
connection. If connection is already established, reuse it."
  `(if (connected-p) (progn ,@body)
       (with-connection (:host ,host :port ,port :encoding ,encoding)
         ,@body)))
       
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
  (close-connection *connection*)
  (setf *connection* nil))

(defun reconnect ()
  "Close and reopen the connection to Redis server."
  (reopen-connection *connection*))

;;; end