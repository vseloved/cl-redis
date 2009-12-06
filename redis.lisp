;;; CL-REDIS implementation of the wire protocol
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

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
  ((raw :initarg :raw :initform (error "Should provide raw Redis output")))
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Redis error: ~a"
                     (slot-value condition 'raw)))))

(defun redis-error (raw)
  "Signal specialised error"
  (error 'redis-error :raw raw))


;; outgoing

(defgeneric tell (type cmd &rest args)
  (:documentation
   "Send command to the server through a socket connection, that might be down.
<_:arg Type /> is for now :inline or :bulk (otherwise error will be signalled).
<_:arg Cmd /> is the command string, <_:arg args /> are arguments. (Keyword ~
arguments are also supported).
In case of :bulk command last argument should be a <_:type sequence />"))

(defmethod tell :before (type cmd &rest args)
  (declare (ignore type cmd args))
  (maybe-connect))

(defmethod tell :after (type cmd &rest args)
  (declare (ignore type cmd args))
  (force-output *redis-out*))

(defmethod tell (type cmd &rest args)
  (declare (ignore cmd args))
  (error "Type ~A commands are not supported." type))

(defmethod tell ((type (eql :bulk)) cmd &rest args)
  (multiple-value-bind (args bulk) (butlast2 args)
    (assert (and bulk (typep bulk 'sequence))
            (bulk)
            "Bulk Redis data must be a sequence")
    (format *redis-out*
            "~a~{ ~a~} ~a~a~a~a"
            cmd args (length bulk) +rtlf+ bulk +rtlf+)))

(defmethod tell ((type (eql :inline)) cmd &rest args)
  (dolist (arg args)
    (assert (not (blankp arg)) () "Redis argument must be non-empty"))
  (format *redis-out* "~a~{ ~a~}~a" cmd args +rtlf+))

(defmethod tell ((type (eql :inline)) (cmd (eql 'SORT)) &rest args)
  (destructuring-bind (key . options) args
    (assert (not (blankp key)) () "Redis argument must be non-empty")
    (format *redis-out*
            "SORT ~a~:[~; BY ~:*~a~]~{ GET ~a~}~:[~; DESC~]~:[~; ALPHA~]~:[~; LIMIT ~:*~{~a~^ ~}~]~a"
            key
            (getf options :by)
            (mklist (getf options :get))
            (getf options :desc)
            (getf options :alpha)
            (getf options :limit)
            +rtlf+)))

(defmethod tell ((type (eql :multi)) cmd &rest args)
  (let ((bulks (cons (string cmd) args)))
    (format *redis-out* "*~a~a" (length bulks) +rtlf+)
    (dolist (bulk bulks)
      (format *redis-out* "$~a~a~a~a"
              (length bulk) +rtlf+ bulk +rtlf+))))

;; ingoing

(defgeneric expect (type)
  (:documentation "Process output of the specified <_:arg type /> from the ~
Redis server"))

(eval-always
  (defmacro def-expect-method (type &body body)
    "Define a specialized EXPECT method"
    (with-gensyms (raw)
      `(defmethod expect ((type (eql ,type)))
         ,(format nil "Process output of type: ~a
A variable _RAW is bound to the output, recieved from the socket"
                  type)
         (maybe-connect)
         (let* ((,raw (read-line *redis-in*))
                (_raw (string-right-trim '(#\Return) (subseq ,raw 1))))
           (when *debug* (format t ,raw))
           (handler-case
               (progn
                 ,@body)
             (error () (redis-error _raw))))))))

(def-expect-method :ok
    (assert (string= _raw "OK"))
  _raw)

(def-expect-method :pong
    (assert (string= _raw "PONG"))
  _raw)

(def-expect-method :inline
    _raw)

(def-expect-method :boolean
    (ecase (char _raw 0)
      (#\0 nil)
      (#\1 t)))

(def-expect-method :integer
    (values (parse-integer _raw)))

(def-expect-method :bulk
    (let ((size (parse-integer _raw)))
      (if (= size -1)
          nil
          (let ((raw (make-array size :element-type 'character)))
            (read-sequence raw *redis-in* :end size)
            (read-char *redis-in*)      ; #\Return
            (read-char *redis-in*)      ; #\Linefeed
            (when *debug* (format t raw))
            raw))))

(def-expect-method :multi
    (let ((n (parse-integer _raw)))
      (if (= n -1)
          nil
          (loop :repeat n :collect (expect :bulk)))))

(defmethod expect ((type (eql :end)))
  ;; do nothing
  )

(defmethod expect ((type (eql :list)))
  (cl-ppcre:split " " (expect :bulk)))
    

;; command definition

(defparameter *cmd-prefix* 'red
  "Prefix for functions, implementing Redis commands")

(defmacro def-cmd (cmd docstring cmd-type (&rest args) reply-type)
  "Define and <_:fun export /> a function for processing the Redis ~
command <_:arg cmd /> with the name <_:var *cmd-redix* />-<_:arg cmd />.
<_:arg Cmd-type /> and <_:arg reply-type /> are the types of ~
respectedly undelying <_:fun tell /> and <_:fun expect />, ~
<_: args /> is the <_:fun tell /> arguments, <_:arg docstring /> ~
is the function's doc itself"
  (let ((cmd-name (intern (format nil "~a-~a" *cmd-prefix* cmd))))
    `(progn
       (defun ,cmd-name (,@args)
         ,docstring
         ,(if-it (position '&rest args)
                 `(apply #'tell ,cmd-type
                         ',cmd
                         ,@(subseq args 0 it)                   
                         ,(nth (1+ it) args))
                 `(tell ,cmd-type ',cmd ,@args))
         (expect ,reply-type))
       (export ',cmd-name :redis))))

;;; end