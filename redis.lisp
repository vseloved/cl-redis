;;; CL-REDIS implementation of the wire protocol
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :redis)


(defparameter +rtlf+ (format nil "~c~c" #\Return #\Linefeed))

(defvar *redis-in* nil "Redis communication stream")
(defvar *redis-out* nil "Redis communication stream")
(defvar *redis-socket* nil)
(defparameter *redis-host* #(127 0 0 1))
(defparameter *redis-port* 6379)

(defvar *debug* nil)


(defun redis-connect (&key debug)
  "Connect to Redis server on <_:var *redis-host* />:<_:var *redis-port* />.
When <_:var debug />, broadcast communication to <_:var *standard-output* />"
  (setf *redis-socket* (socket-connect *redis-host* *redis-port*)
        *redis-in* (socket-stream *redis-socket*)
        *redis-out* (if (setf *debug* debug)
                        (make-broadcast-stream (socket-stream *redis-socket*)
                                               *standard-output*)
                        (socket-stream *redis-socket*))))

(defun maybe-connect ()
  "Unless we're already connected, redis-connect.
The logic of the whole Redis connection process is, that you don't
disconnect from it, just the socket may be closed in some circumstances:
the thread finishes execution, timeout occurs. If we still need to get some
data from the server, we'll reconnect"
  (unless (and *redis-in*
               (handler-case (let ((test (read-char-no-hang *redis-in*)))
                               (unread-char test *redis-in*)
                               t)
                 (error () nil)))
    (redis-connect)))


;; conditions

(define-condition redis-error (error)
  ((raw :initarg :raw :initform '(error "Should provide raw Redis output")))
  (:report (lambda (c stream)
             (format stream "Redis error: ~a"
                     (slot-value condition 'raw)))))

(defun redis-error (raw)
  "Signal specialised error"
  (error 'redis-error :raw raw))


;; outgoing

(defun tell (type cmd &rest args)
  "Send command to the server through a socket connection, that might be down.
<_:arg Type /> is for now :inline or :bulk (otherwise error will be signalled).
<_:arg Cmd /> is the command string, <_:arg args /> are arguments.
In case of :bulk command last argument should be a <_:type sequence />"
  (let (bulk)
    (if (eq type :bulk)
        (progn (multiple-value-setq (args bulk) (butlast2 args))
               (assert (and bulk (typep bulk 'sequence))
                       (bulk)
                       "Bulk Redis data should be a sequence"))
        (mapc (lambda (arg) (assert (not (blankp arg)) ()
                                    "Redis argument should not be empty"))
              args))
    (maybe-connect)
    (format *redis-out* "~a~{ ~a~}" cmd args)
    (ecase type
      (:inline (format *redis-out* +rtlf+))
      (:bulk (format *redis-out* " ~a~a~a~a"
                     (length bulk)
                     +rtlf+
                     bulk
                     +rtlf+)))
    (force-output *redis-out*)))


;; ingoing

(defgeneric expect (type)
  (:documentation "Process output of the specified <_:arg type /> from the ~
Redis server"))

(eval-always
  (defmacro def-expect-method (type &body body)
    "Define a specialized EXPECT method"
    `(defmethod expect ((type (eql ,type)))
       ,(format nil "Process output of type: ~a
A variable _RAW is bound to the output, recieved from the socket"
                type)
       (maybe-connect)
       ,(with-gensyms (raw)
         `(let* ((,raw (read-line *redis-in*))
                 (_raw (subseq ,raw 1)))
            (when *debug* (format t ,raw))
            ,@body)))))

(def-expect-method :ok
    (or (string= _raw "OK")
        (redis-error _raw)))

(def-expect-method :pong
    (or (string= _raw "PONG")
        (redis-error _raw)))

(def-expect-method :inline
  (subseq _raw 0 (1- (length _raw))))

(def-expect-method :boolean
    (case (elt _raw 0)
      (#\0 nil)
      (#\1 t)
      (otherwise (redis-error _raw))))

(def-expect-method :integer
    (cond
      ((string= (subseq _raw 0 1) "-1") nil)
      ((digit-char-p (elt _raw 0)) (values (parse-integer _raw)))
      (t (redis-error _raw))))

(def-expect-method :bulk
    (handler-case (let ((size (parse-integer _raw)))
                    (and (> size -1)
                         (let ((raw (read-line *redis-in*)))
                           (when *debug* (format t raw))
                           (subseq raw 0 size))))
      (error () (redis-error _raw))))

(def-expect-method :multi
    (handler-case (let ((n (parse-integer _raw)))
                    (loop :repeat n
                       :collect (expect :bulk)))
      (error () (redis-error _raw))))

(defmethod expect ((type (eql :end)))
  ;; do nothing
  )

(defmethod expect ((type (eql :list)))
  (cl-ppcre:split " " (expect :bulk)))
    

;; command definition

(defparameter *cmd-prefix* "red"
  "Prefix for functions, implementing Redis commands")

(defmacro def-cmd (cmd docstring cmd-type (&rest args) reply-type)
  "Define and <_:fun export /> a function for processing the Redis ~
command <_:arg cmd /> with the name <_:var *cmd-redix* />-<_:arg cmd />.
<_:arg Cmd-type /> and <_:arg reply-type /> are the types of ~
respectedly undelying <_:fun tell /> and <_:fun expect />, ~
<_: args /> is the <_:fun tell /> arguments, <_:arg docstring /> ~
is the function's doc itself"
  (let ((cmd-name (mksym cmd :format (strcat *cmd-prefix* "-~a"))))
    `(progn
       (defun ,cmd-name (,@args)
         ,docstring
         ,(if-it (position '&rest (reverse args))
                 `(apply #'tell ,cmd-type
                         ,(string cmd)
                         ,@(butlast args (1+ it))
                         ,(last1 args it))
                 `(tell ,cmd-type ,(string cmd) ,@args))
         (expect ,reply-type))
       (export ',cmd-name :redis))))

;;; end