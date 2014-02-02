;;; CL-REDIS connection handling
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :redis)


(eval-always (defparameter +utf8+ '(:utf-8 :eol-style :crlf)))


;;; Debugging support

(defvar *echo-p* nil
  "Whether the server-client communication should be echoed to the
stream specified by *ECHO-STREAM*.  The default is NIL, meaning no
echoing.")

(defvar *echo-stream* *standard-output*
  "A stream to which the server-client communication will be echoed
for debugging purposes.  The default is *STANDARD-OUTPUT*.")


;;; Low-level connection handling

(defvar *connection* nil "The current Redis connection.")

(defclass redis-connection ()
  ((host
    :initarg  :host
    :initform #(127 0 0 1)
    :reader   conn-host)
   (port
    :initarg  :port
    :initform 6379
    :reader   conn-port)
   (auth
    :initarg  :auth
    :initform nil
    :reader conn-auth)
   (socket
    :initform nil
    :accessor conn-socket)
   (stream
    :initform nil
    :accessor conn-stream))
  (:documentation "Representation of a Redis connection."))

(defmethod initialize-instance :after ((conn redis-connection) &key)
  (open-connection conn))

(defun connection-open-p (conn)
  "Is the socket of CONNECTION open?"
  (and-it (conn-stream conn)
          (open-stream-p it)))

(defun open-connection (conn)
  "Create a socket connection to the host and port of CONNECTION and
set the socket of CONN to the associated socket."
  (setf (conn-stream conn)
        (flex:make-flexi-stream (usocket:socket-stream
                                 (setf (conn-socket conn)
                                       (usocket:socket-connect
                                        (conn-host conn) (conn-port conn)
                                        :element-type 'flex:octet)))
                                :external-format +utf8+
                                #-lispworks :element-type
                                #-lispworks 'flex:octet))
  (when (conn-auth conn)
    (let ((*connection* conn)) ; AUTH needs *CONNECTION* to be bound
                               ; to the current connection.  At this
                               ; stage, *CONNECTION* is not bound yet.
      (auth (conn-auth conn)))))

(defun close-connection (conn)
  "Close the socket of CONN."
  (when (connection-open-p conn)
    (handler-case
        (usocket:socket-close (conn-socket conn))
      (error (e)
        (warn "Ignoring the error that happened while trying to close ~
Redis socket: ~A" e)))))

(defun reopen-connection (conn)
  "Close and reopen CONN."
  (close-connection conn)
  (open-connection conn))


;;; Top-level API

(defun connected-p ()
  "Is there a current connection?"
  (and *connection* (connection-open-p *connection*)))

(defun connect (&key (host #(127 0 0 1)) (port 6379) auth)
  "Connect to Redis server."
  (when (connected-p)
    (restart-case (error 'redis-error
                         :error "A connection to Redis server is already established.")
      (:leave ()
        :report "Leave it."
        (return-from connect))
      (:replace ()
        :report "Replace it with a new connection."
        (disconnect))))
  (setf *connection* (make-instance 'redis-connection
                                    :host host :port port :auth auth)))


(defun disconnect ()
  "Disconnect from Redis server."
  (when *connection*
    (close-connection *connection*)
    (setf *connection* nil)))

(defun reconnect ()
  "Close and reopen the connection to Redis server."
  (reopen-connection *connection*))

(defmacro with-connection ((&key (host #(127 0 0 1))
                                 (port 6379)
                                 auth)
                           &body body)
  "Evaluate BODY with the current connection bound to a new connection
specified by the given HOST and PORT"
  `(let ((*connection* (make-instance 'redis-connection
                                      :host ,host :port ,port :auth ,auth)))
     (unwind-protect (progn ,@body)
       (disconnect))))


;;; Handling connection errors

(defmacro reconnect-restart-case ((&key error comment) &body body)
  "Signal the condition of type REDIS-CONNECTION-ERROR denoted by
the given ERROR and COMMENT offering a :RECONNECT restart to re-evaluate BODY."
  `(if *pipelined*
       ;; don't intercept connection-errors inside a pipeline -
       ;; it will be done on the highest level of a pipeline to allow
       ;; the whole pipeline (with possible nestsed pipelines) to restart
       (progn ,@body)
       (restart-case (error 'redis-connection-error
                            :error ,error :message ,comment)
         (:reconnect ()
           :report "Try to reconnect and repeat action."
           (reconnect)
           ,@body))))

(defmacro with-reconnect-restart (&body body)
  "When, during the execution of BODY, an error occurs that breaks
the connection, a REDIS-CONNECTION-ERROR is signalled,
offering a :RECONNECT restart that will re-evaluate body after
the conenction is re-established."
  (with-gensyms (e)
    `(handler-case (progn ,@body)
       (usocket:connection-refused-error (,e)
         ;; Errors of this type commonly occur when there is no Redis server
         ;; running, or when one tries to connect to the wrong host or port.
         (reconnect-restart-case
           (:error ,e
            :comment "Make sure Redis server is running and check your connection parameters.")
           ,@body))
       ((or usocket:socket-error stream-error end-of-file
            #+lispworks comm:socket-error) (,e)
         (reconnect-restart-case (:error ,e)
           ,@body)))))


;;; Convenience macros

(defmacro with-recursive-connection ((&key (host #(127 0 0 1))
                                           (port 6379)
                                           auth)
                                     &body body)
  "Execute BODY with *CONNECTION* bound to the default Redis
connection. If connection is already established, reuse it."
  `(if (connected-p)
       (progn ,@body)
       (with-connection (:host ,host :port ,port :auth ,auth)
         ,@body)))

(defmacro with-persistent-connection ((&key (host #(127 0 0 1))
                                            (port 6379)
                                            auth)
                                      &body body)
  "Execute BODY inside WITH-CONNECTION. But if connection is broken
due to REDIS-CONNECTION-ERROR (a network error or timeout),
transparently reopen it."
  `(with-connection (:host ,host :port ,port :auth ,auth)
     (handler-bind ((redis-connection-error
                     (lambda (e)
                       (declare (ignore e))
                       (warn "Reconnecting to Redis.")
                       (invoke-restart :reconnect))))
       ,@body)))

;;; end
