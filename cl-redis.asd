;;; CL-REDIS system definition
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk.  See LICENSE file for permissions

(in-package :asdf)

(defsystem #:cl-redis
  :version "2.1.1"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Redis database client, using iolib interface."
  :depends-on (#:rutils #:cl-ppcre #:babel #:iolib)
  :serial t
  :components ((:file "package")
               (:file "connection")
               (:file "redis")
               (:file "commands")))


#+nuts
(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis))))
  (operate 'load-op '#:cl-redis)
  (operate 'test-op '#:cl-redis-test))

#+nuts
(defsystem #:cl-redis-test
  :version "2.0.0"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "CL-Redis test suite"
  :depends-on (#:cl-redis #:bordeaux-threads #:flexi-streams
               (:version #:nuts "0.4.0"))
  :components ((:file "test")))

#+nuts
(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis-test))))
  (operate 'load-op '#:cl-redis-test)
  (funcall (intern (symbol-name 'run-all-tests)
                   '#:redis-test)))

;;; end
