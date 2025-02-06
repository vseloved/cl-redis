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

;; First, define the SSL stream methods
(defmacro define-ssl-stream-methods ()
  `(progn
     ;; For SSL streams
     (defmethod trivial-gray-streams:stream-clear-input ((stream cl+ssl::ssl-stream))
       (trivial-gray-streams:stream-clear-input (cl+ssl::ssl-stream-socket stream)))
     
     (defmethod trivial-gray-streams:stream-read-sequence 
         ((stream cl+ssl::ssl-stream) sequence start end &rest args)
       (declare (ignore args))
       (handler-case
           (let ((bytes-read 0))
             (loop while (< bytes-read (- end start))
                   do (let ((byte (read-byte stream nil nil)))
                        (when (null byte)
                          (return bytes-read))
                        (setf (elt sequence (+ start bytes-read)) byte)
                        (incf bytes-read)))
             bytes-read)
         (cl+ssl::ssl-error-zero-return () bytes-read)
         (cl+ssl::ssl-error-syscall () -1)
         (cl+ssl::ssl-error-ssl () -1)))
     
     (defmethod trivial-gray-streams:stream-write-sequence 
         ((stream cl+ssl::ssl-stream) sequence start end &rest args)
       (declare (ignore args))
       (handler-case
           (loop for i from start below end
                 do (write-byte (elt sequence i) stream)
                 finally (return sequence))
         (cl+ssl::ssl-error-zero-return () sequence)
         (cl+ssl::ssl-error-syscall () sequence)
         (cl+ssl::ssl-error-ssl () sequence)))

     ;; For regular streams
     (defmethod trivial-gray-streams:stream-clear-input ((stream stream))
       (clear-input stream))
     
     (defmethod trivial-gray-streams:stream-read-sequence 
         ((stream stream) sequence start end &rest args)
       (declare (ignore args))
       (read-sequence sequence stream :start start :end end))
     
     (defmethod trivial-gray-streams:stream-write-sequence 
         ((stream stream) sequence start end &rest args)
       (declare (ignore args))
       (write-sequence sequence stream :start start :end end)
       sequence)))

;; Execute the macro to define the methods
(define-ssl-stream-methods)

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
    :accessor conn-stream)
   (ssl
    :initarg :ssl
    :initform nil
    :reader conn-ssl)
   (verify
    :initarg :verify
    :initform nil
    :reader conn-verify)
   (certificate
    :initarg :certificate
    :initform nil
    :reader conn-certificate)
   (key
    :initarg :key
    :initform nil
    :reader conn-key)
   (cipher-list
    :initarg :cipher-list
    :initform nil
    :reader conn-cipher-list))
  (:documentation "Representation of a Redis connection."))

(defmethod initialize-instance :after ((conn redis-connection) &key)
  (open-connection conn))

(defmethod conn-stream ((object null))
  (error 'redis-connection-error
	 :error "No connection to Redis server was not established."))

(defun connection-open-p (conn)
  "Is the socket of CONNECTION open?"
  (and-it (conn-stream conn)
          (open-stream-p it)))

(defun open-connection (conn)
  "Create a socket connection to the host and port of CONNECTION and
set the socket of CONN to the associated socket."
  (let ((socket (usocket:socket-connect (conn-host conn) (conn-port conn)
                                         :element-type 'flex:octet)))
    (setf (conn-socket conn) socket)
    (let ((socket-stream (usocket:socket-stream socket)))
      (setf (conn-stream conn)
            (flex:make-flexi-stream
             (if (conn-ssl conn)
                 (cl+ssl:make-ssl-client-stream
                  socket-stream
                  :verify (conn-verify conn)
                  :certificate (conn-certificate conn)
                  :key (conn-key conn)
                  :cipher-list (conn-cipher-list conn))
                 socket-stream)
             :external-format +utf8+
             #-lispworks :element-type
             #-lispworks 'flex:octet))))
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

(defun connect (&key (host #(127 0 0 1))
                     (port 6379)
                     auth
                     ssl
                     verify
                     certificate
                     key
                     cipher-list)
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
                                    :host host
                                    :port port
                                    :auth auth
                                    :ssl ssl
                                    :verify verify
                                    :certificate certificate
                                    :key key
                                    :cipher-list cipher-list)))


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
                                 auth
                                 ssl
                                 verify
                                 certificate
                                 key
                                 cipher-list)
                           &body body)
  "Evaluate BODY with the current connection bound to a new connection
specified by the given HOST and PORT"
  `(let ((*connection* (make-instance 'redis-connection
                                      :host ,host
                                      :port ,port
                                      :auth ,auth
                                      :ssl ,ssl
                                      :verify ,verify
                                      :certificate ,certificate
                                      :key ,key
                                      :cipher-list ,cipher-list)))
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
                                           auth
                                           ssl
                                           verify
                                           certificate
                                           key
                                           cipher-list)
                                     &body body)
  "Execute BODY with *CONNECTION* bound to the default Redis
connection. If connection is already established, reuse it."
  `(if (connected-p)
       (progn ,@body)
       (with-connection (:host ,host
                         :port ,port
                         :auth ,auth
                         :ssl ,ssl
                         :verify ,verify
                         :certificate ,certificate
                         :key ,key
                         :cipher-list ,cipher-list)
         ,@body)))

(defmacro with-persistent-connection ((&key (host #(127 0 0 1))
                                            (port 6379)
                                            auth
                                            ssl
                                            verify
                                            certificate
                                            key
                                            cipher-list)
                                      &body body)
  "Execute BODY inside WITH-CONNECTION. But if connection is broken
due to REDIS-CONNECTION-ERROR (a network error or timeout),
transparently reopen it."
  `(with-connection (:host ,host
                     :port ,port
                     :auth ,auth
                     :ssl ,ssl
                     :verify ,verify
                     :certificate ,certificate
                     :key ,key
                     :cipher-list ,cipher-list)
     (handler-bind ((redis-connection-error
                     (lambda (e)
                       (declare (ignore e))
                       (warn "Reconnecting to Redis.")
                       (invoke-restart :reconnect))))
       ,@body)))

;;; end
