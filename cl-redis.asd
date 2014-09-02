;;; CL-REDIS system definition
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk.  See LICENSE file for permissions

(in-package :asdf)

(defsystem #:cl-redis
  :version "2.3.7"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>, Oleksandr Manzyuk <manzyuk@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "A fast and robust Common Lisp client for Redis"
  :depends-on (#:rutils #:cl-ppcre #:usocket #:flexi-streams #:babel)
  :serial t
  :components ((:file "package")
               (:file "float")
               (:file "connection")
               (:file "redis")
               (:file "commands")))


(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis))))
  (operate 'load-op '#:cl-redis)
  (operate 'test-op '#:cl-redis-test))

(defsystem #:cl-redis-test
  :version "2.3.7"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "CL-Redis test suite"
  :depends-on (#:cl-redis #:bordeaux-threads #:flexi-streams #:should-test)
  :components ((:file "test")))

(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis-test))))
  (asdf:load-system '#:cl-redis-test)
  (funcall (read-from-string "cl-redis-test:run-tests")))

;;; end
