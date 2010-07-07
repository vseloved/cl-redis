;;; CL-REDIS system definition
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk.  See LICENSE file for permissions

(in-package :asdf)

(defsystem #:cl-redis
  :version '(1 5 0)
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Redis database client, using usocket interface."
  :depends-on (:rutils :usocket :cl-ppcre :flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "connection")
               (:file "redis")
               (:file "commands")))


#+nuts
(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis))))
  (operate 'load-op '#:cl-redis)
  (operate 'test-op '#:cl-redis-test :force t))

#+nuts
(defsystem #:cl-redis-test
  :version '(1 5 0)
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "CL-Redis test suite"
  :depends-on (:cl-redis :nuts :bordeaux-threads)
  :serial t
  :components ((:file "test")))

#+nuts
(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis-test))))
  (operate 'load-op '#:cl-redis-test)
  (funcall (intern (symbol-name 'run-tests)
                   '#:redis-test)))

;;; end
