;;; CL-REDIS package definition
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :cl-user)

(defpackage :redis
  (:use :common-lisp :rutils.user :rutils.short :usocket :flexi-streams
        #+:nuts :nuts)
  (:export #:*redis-in*
           #:*redis-host*
           #:*redis-out*
           #:*redis-port*

           #:*cmd-prefix*

           #:def-cmd
           #:def-expect-method
           #:expect
           #:redis-connect
           #:tell

           #:redis-error))

;;; end