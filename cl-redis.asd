;;; CL-REDIS system definition
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk.  See LICENSE file for permissions

(in-package :asdf)

(defsystem #:cl-redis
  :name "Redis client"
  :version '(1 3 4)
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "A Redis database interface through socket"
  :depends-on (:rutils :usocket :cl-ppcre :flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "connection")
               (:file "redis")
               (:file "commands")))


#+:nuts
(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis))))
  (operate 'load-op '#:cl-redis)
  (operate 'test-op '#:cl-redis-test :force t))

#+:nuts
(defsystem #:cl-redis-test
  :name "Redis client testsuite"
  :version '(1 0 0)
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description ""
  :depends-on (:cl-redis :nuts)
  :serial t
  :components ((:file "test")))

#+:nuts
(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis-test))))
  (operate 'load-op '#:cl-redis-test)
  (funcall (intern (symbol-name 'run-tests)
                   '#:redis-test)))

;;; end
