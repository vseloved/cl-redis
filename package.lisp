;;; CL-REDIS package definition
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :cl-user)

(defpackage :redis
  (:use :common-lisp :rutils.user :rutils.short :usocket :flexi-streams
        #+:nuts :nuts)
  (:export #:connect
           #:disconnect
           #:connected-p
           #:with-connection

           #:*echo-p*
           #:*echo-stream*

           #:*cmd-prefix*

           #:def-cmd
           #:def-expect-method
           #:expect
           #:tell

           #:redis-error
           #:redis-bad-reply
           #:redis-error-reply
           #:redis-connection-error)) 
;;; end