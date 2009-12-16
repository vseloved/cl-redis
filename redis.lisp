;;; CL-REDIS implementation of the wire protocol
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :redis)

(defvar *redis-stream* nil
  "Redis communication stream.")

(defparameter *redis-host* #(127 0 0 1)
  "Default Redis connection host.")

(defparameter *redis-port* 6379
  "Default Redis connection port.")

(defparameter *redis-external-format* (make-external-format :utf-8 :eol-style :crlf))

(defvar *echo-p* nil
  "Whether the server-client communication should be echoed to the
stream specified by *ECHO-STREAM*.  The default is NIL, meaning no
echoing.")

(defvar *echo-stream* *standard-output*
  "A stream to which the server-client communication will be echoed
for debugging purposes.  The default is *standard-output*.")

;; utils

(defun byte-length (string)
  "Return the length of STRING in octets encoded using *REDIS-EXTERNAL-FORMAT*."
  (octet-length string :external-format *redis-external-format*))

(defun write-redis-line (fmt &rest args)
  (let ((string (apply #'format nil fmt args)))
    (when *echo-p* (format *echo-stream* "C: ~A~%" string))
    (write-line string *redis-stream*)))

;; conditions

(define-condition redis-error (error)
  ((message :initarg :message :reader redis-error-message))
  (:report (lambda (e stream)
             (format stream "Redis error: ~A" (redis-error-message e))))
  (:documentation "This is the condition type that will be used to
signal virtually all Redis-related errors."))

(define-condition redis-connection-error (redis-error)
  ()
  (:documentation "Conditions of this type are signaled when
errors occur that break the connection stream.  They offer a
:reconnect restart."))

(define-condition redis-error-reply (redis-error)
  ()
  (:documentation "Raised when an error reply is received from the
Redis server."))

(define-condition redis-protocol-error (redis-error)
  ()
  (:documentation "Raised when a Redis protocol error is detected."))

(defun wrap-connection-error (e)
  (make-instance 'redis-connection-error
                 :message (princ-to-string e)))

;; connect

(defun redis-connect ()
  (handler-case
      (setf *redis-stream* (make-flexi-stream
                            (socket-stream
                             (socket-connect *redis-host*
                                             *redis-port*
                                             :element-type 'octet))
                            :external-format *redis-external-format*
                            :element-type 'octet))
    ((or socket-error stream-error)
        (e)
      (restart-case (error (wrap-connection-error e))
        (:reconnect () :report "Try to reconnect." (redis-connect))))))
  

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
  (force-output *redis-stream*))

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
    (write-redis-line "*~A" (byte-length bulks))
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
    (with-gensyms (line char)
      `(defmethod expect ((type (eql ,type)))
         ,(format nil "Process output of type ~a.
A variable REPLY is bound to the output, recieved from the socket."
                  type)
         (let* ((,line (read-line *redis-stream*))
                (,char (char ,line 0))
                (reply (subseq ,line 1)))
           (when *echo-p* (format *echo-stream* "S: ~A~%" ,line))
           (case ,char
             ((#\-)
              (error 'redis-error-reply :message reply))
             ((#\+ #\: #\$ #\*)
              (progn ,@body))
             (otherwise
              (error 'redis-protocol-error
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
        (let ((octets (make-array size :element-type 'octet)))
          (read-sequence octets *redis-stream*)
          (read-byte *redis-stream*)    ; #\Return
          (read-byte *redis-stream*)    ; #\Linefeed
          (let ((string (octets-to-string octets
                                          :external-format *redis-external-format*)))
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

(defmacro with-reconnect-restart (&body body)
  (with-unique-names (body-name)
    `(labels ((,body-name ()
                (handler-case (progn ,@body)
                  ((or socket-error stream-error)
                      (e)
                    (restart-case (error (wrap-connection-error e))
                      (:reconnect
                          ()
                        :report "Try to reconnect."
                        (redis-connect)
                        (,body-name)))))))
       (,body-name))))

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
         (with-reconnect-restart
           ,(if-it (position '&rest args)
                   `(apply #'tell ,cmd-type
                           ',cmd
                           ,@(subseq args 0 it)                   
                           ,(nth (1+ it) args))
                   `(tell ,cmd-type ',cmd ,@args))
           (expect ,reply-type)))
       (export ',cmd-name :redis))))

;;; end