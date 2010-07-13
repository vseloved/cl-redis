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
   (socket
    :initform nil
    :accessor connection-socket))
  (:documentation "Representation of a Redis connection."))

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
the connection socket, a condition of type REDIS-CONNECTION-ERROR is raised
offering a :reconnect restart whose body is given by BODY."
  (with-gensyms (err body-fn)
    `(flet ((,body-fn () ,@body))
       (handler-case ,expression
         (sockets:socket-connection-refused-error (,err)
           ;; Errors of this type commonly occur when there is no Redis server
           ;; running, or when one tries to connect to the wrong host or port.
           ;; We anticipate this by providing a helpful comment.
           (signal-connection-error-with-reconnect-restart
            :message ,err
            :comment "Make sure Redis server is running and check your connection parameters."
            :restart (,body-fn)))
         ((or sockets:socket-error stream-error) (,err)
           (signal-connection-error-with-reconnect-restart
            :message ,err
            :restart (,body-fn)))))))

(defmethod initialize-instance :after ((connection redis-connection) &key)
  (open-connection connection))
  
(defun open-connection (connection)
  "Create a socket connection to the host and port of CONNECTION and
set the socket of CONNECTION to the associated socket."
  (provide-reconnect-restart
      (let ((socket (sockets:make-socket
                     :connect :active
                     :address-family :internet
                     :type :stream
                     :external-format '(:utf-8 :eol-style :crlf)
                     :ipv6 nil)))
        (setf (connection-socket connection)
              (sockets:connect socket (sockets:lookup-host
                                       (connection-host connection))
                               :port (connection-port connection))))
    (open-connection connection)))

(defun open-connection-p (connection)
  "Is the socket of CONNECTION open?"
  (and-it (connection-socket connection)
          (sockets:socket-open-p it)))

(defun close-connection (connection)
  "Close the socket of CONNECTION."
  (when (open-connection-p connection)
    (handler-case
        (close (connection-socket connection))
      (error (e)
        (warn "Ignoring the error that happened while trying to close Redis communication socket: ~A" e)))))

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
  "When, inside BODY, an error occurs that breaks the socket of CONNECTION,
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
                                 (port 6379))
                           &body body)
  "Evaluate BODY with the current connection bound to a new connection
specified by the given HOST and PORT"
  `(let ((*connection* (make-instance 'redis-connection
                                      :host ,host
                                      :port ,port)))
     (unwind-protect (progn ,@body)
       (close-connection *connection*))))

(defmacro with-recursive-connection ((&key (host #(127 0 0 1))
                                           (port 6379))
                                     &body body)
  "Execute BODY with *CONNECTION* bound to the default Redis
connection. If connection is already established, reuse it."
  `(if (connected-p) (progn ,@body)
       (with-connection (:host ,host :port ,port)
         ,@body)))
       
(defun connected-p ()
  "Is the current connection to Redis server still open?"
  (and *connection* (open-connection-p *connection*)))

(defun connect (&key (host #(127 0 0 1)) (port 6379))
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
                                    :port port)))

(defun disconnect ()
  "Disconnect from Redis server."
  (close-connection *connection*)
  (setf *connection* nil))

(defun reconnect ()
  "Close and reopen the connection to Redis server."
  (reopen-connection *connection*))

;;; end