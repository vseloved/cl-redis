;;; CL-REDIS testsuite package definition
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :cl-user)

(defpackage #:redis-test
  (:use :common-lisp :rutils.user :rutils.short #+:nuts :nuts
        :redis))

(in-package #:redis-test)


(define-symbol-macro +rtlf+ redis::+rtlf+)

#+nil
(deftest connect ()
  )

#+nil
(deftest tell ()
  (check string=
         (with-output-to-string (*redis-out*)
           (tell :inline "PING"))
         (format nil "PING~a" +rtlf+))
  (check string=
         (with-output-to-string (*redis-out*)
           (tell :bulk "SET" "a" "123"))
         (format nil "SET a 3~a123~a" +rtlf+ +rtlf+)))


(deftest expect ()
  (with-input-from-string (in
"+OK
+PONG
+10$
+0$
+10
+3
abc
+2
$1
a
$0

+5
a b c")
    (let ((*redis-in* (make-two-way-stream in *redis-out*)))
      (cumulative-and
       (check true                 (expect :ok))
       (check true                 (expect :pong))
       (check string= "10$"        (expect :inline))
       (check null                 (expect :boolean))
       (check = 10                 (expect :integer))
       (check string= "abc"        (expect :bulk))
       (check equal '("a" "")      (expect :multi))
       (check equal '("a" "b" "c") (expect :list))))))

(defun find-s (seq str)
  (find str seq :test #'string=))

(deftest commands ()
  (check true              (red-ping))
  (check true              (red-select 15)) ; select last DB index
  (check true              (red-flushdb))
  #+nil (red-quit)
  #+nil (red-auth)
  (check true              (red-set "y" "1"))
  (check string= "1"       (red-getset "y" "2"))
  (check string= "2"       (red-get "y"))
  (check true              (red-set "z" "3"))
  (check equal '("2" "3")  (red-mget "y" "z"))
  (check equal '("2" nil)  (red-mget "y" "a"))
  (check null              (red-setnx "z" "3"))
  (check true              (red-setnx "u" "3"))
  (check = 4               (red-incr "u"))
  (check = 6               (red-incrby "u" 2))
  (check = 5               (red-decr "u"))
  (check = 3               (red-decrby "u" 2))
  (check true              (red-exists "u"))
  (check null              (red-exists "v"))
  (check true              (red-del "u"))
  (check null              (red-exists "u"))
  (check string= "none"    (red-type "u"))
  (check string= "string"  (red-type "z"))
  (check equal '("y" "z")  (red-keys "*"))
  #+nil (red-randomkey)
  (check true              (red-rename "z" "a"))
  (check string= "3"       (red-get "a"))
  (check null              (red-renamenx "y" "a"))
  (check true              (red-renamenx "y" "b"))
  (check-errs              (red-renamenx "b" "b"))
  (check = 2               (red-dbsize))
  (check true              (red-expire "b" 1))
  (check null              (progn (sleep 1)
                                  (red-get "y")))
  (check null              (red-expire "b" 1))
  (check true              (red-expire "a" 600))
  (check < 595             (red-ttl "a"))
  (check true              (red-rpush "l" "1"))
  (check true              (red-rpush "l" "1"))
  (check true              (red-rpush "l" "1"))
  (check = 2               (red-lrem "l" 0 "1"))
  (check = 0               (red-lrem "l" 0 "a"))
  (check true              (red-lpush "l" "0"))
  (check = 2               (red-llen "l"))
  (check equal '("0" "1")  (red-lrange "l" 0 1))
  (check equal '("0")      (red-lrange "l" 0 0))
  (check equal '("0" "1")  (red-lrange "l" 0 2))
  (check equal '("0" "1")  (red-lrange "l" 0 10))
  (check equal '("1")      (red-lrange "l" 1 1))
  (check null              (red-lrange "l" 2 1))
  (check null              (red-lrange "l" 2 3))
  (check string= "0"       (red-lindex "l" 0))
  (check true              (red-lset "l" 0 "a"))
  (check equal '("a" "1")  (red-lrange "l" 0 10))
  (check equal '("a")      (red-ltrim "l" 0 0))
  (check null              (red-ltrim "l" 2 3))
  (check true              (red-lpush "l" "2"))
  (check true              (red-rpush "l" "3"))
  (check true              (red-rpush "l" "4"))
  (check string= "2"       (red-lpop "l"))
  (check string= "4"       (red-rpop "l"))
  (check-errs              (red-get "l"))
  (check true              (red-sadd "s" "1"))
  (check null              (red-sadd "s" "1"))
  (check true              (red-sadd "s" "2"))
  (check find-s '("2" "1") (red-spop "s"))
  (check true              (or (red-sadd "s" "2")
                               (red-sadd "s" "1")))
  (check true              (red-srem "s" "1"))
  (check string= "2"       (red-spop "s"))
  (check null              (red-spop "s"))
  (check true              (red-sadd "s" "2"))
  (check true              (red-sismember "s" "2"))
  (check true              (red-sadd "s" "1"))
  (check true              (red-smove "s" "s2" "1"))
  (check true              (red-sismember "s2" "1"))
  (check null              (red-smove "s" "s2" "3"))
  (check null              (red-sismember "s2" "3"))
  (check true              (red-sadd "s" "1"))
  (check true              (red-smove "s" "s2" "1"))
  (check = 1               (red-scard "s"))
  (check null              (red-sinter "s" "s2"))
  (check true              (red-sadd "s" "1"))
  (check equal '("1")      (red-sinter "s" "s2"))
  (check true              (red-sinterstore "s3" "s" "s2"))
  (check equal '("1")      (red-smembers "s3"))
  (check equal '("1" "2")  (red-sunion "s" "s2"))
  (check true              (red-sunionstore "s4" "s" "s2"))
  (check equal '("1" "2")  (red-smembers "s4"))
  (check equal '("2")      (red-sdiff "s4" "s3"))
  (check true              (red-sdiffstore "s5" "s4" "s3"))
  (check equal '("2")      (red-smembers "s5"))
  #+nil (red-move)
  #+nil (red-flushall)
  #+nil (red-sort)
  (check true              (red-save))
  (check true              (red-bgsave))
  (check integerp          (red-lastsave))
  #+nil (red-shutdown)
  #+nil (red-info)
  #+nil (red-monitor)
  #+nil (red-slaveof))
   

(defun run-tests (&key debug)
  (terpri)
  (princ "Runnning CL-REDIS tests... ")
  (redis-connect :debug debug)
  (princ (if (every (lambda (rez) (every 'true (mklist rez)))
                    (run-test #+nil tell
                              expect
                              commands))
             "OK"
             (format nil "some tests failed. See log file for details: ~a"
                     *logg-out*)))
  (terpri)
  (terpri))
      

;;; end