;;; CL-REDIS testsuite package definition
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :cl-user)

(defpackage #:redis-test
  (:use :common-lisp :rutils.user :rutils.short #+:nuts :nuts
        :redis))

(in-package #:redis-test)


(define-symbol-macro +rtlf+ redis::+rtlf+)

#+nil  ;; TODO: make the proper isolated test, that will not touch maybe-connect
(deftest tell ()
  (check string=
         (with-output-to-string (*redis-out*)
           (tell :inline "PING"))
         (format nil "PING~a" +rtlf+))
  (check string=
         (with-output-to-string (*redis-out*)
           (tell :bulk "SET" "a" "123"))
         (format nil "SET a 3~a123~a" +rtlf+ +rtlf+)))


(defun expect-from-str (expected input)
  (with-input-from-string (in (strcat input +rtlf+))
    (let ((*redis-in* (make-two-way-stream in *redis-out*)))
      (expect expected))))
  
(deftest expect ()
  (check true            (expect-from-str :ok "+OK"))
  (check true            (expect-from-str :pong "+PONG"))
  (check string= "10$"   (expect-from-str :inline "+10$"))
  (check null            (expect-from-str :boolean "+0$"))
  (check = 10            (expect-from-str :integer "+10"))
  (check string= "abc"   (expect-from-str :bulk (format nil "+3~aabc" +rtlf+)))
  (check equal '("a" "") (expect-from-str :multi
                                          (format nil "+2~a$1~aa~a$0~a"
                                                  +rtlf+ +rtlf+ +rtlf+ +rtlf+)))
  (check equal '("a" "b" "c")
                         (expect-from-str :list
                                          (format nil "+5~aa b c"
                                                  +rtlf+))))

(defun find-s (seq str)
  (true (find str seq :test #'string=)))

(defmacro with-test-db (&body body)
  `(cumulative-and
    (check true (red-ping))
    (check true (red-select 15))
    (check true (red-flushdb))
    ,@body
    (check true (red-flushdb))))

(deftest commands ()
  (with-test-db
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
    (check true              (red-rename "z" "a"))
    (check string= "3"       (red-get "a"))
    (check null              (red-renamenx "y" "a"))
    (check true              (red-renamenx "y" "b"))
    (check-errs              (red-renamenx "b" "b"))
    (check = 2               (red-dbsize))
    (check true              (red-expire "b" 1))
    (check null              (progn (sleep 2)
                                    (red-get "b")))
    (check null              (red-expire "b" 1))
    (check string= "a"       (red-randomkey))
    (check true              (red-expire "a" 600))
    (check < 595             (red-ttl "a"))
    (check true              (red-rpush "l" "1"))
    (check true              (red-rpush "l" "1"))
    (check true              (red-rpush "l" "1"))
    (check = 3               (red-lrem "l" 0 "1"))
    (check = 0               (red-lrem "l" 0 "a"))
    (check true              (red-lpush "l" "1"))
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
    (check true              (red-ltrim "l" 0 0))
    (check equal '("a")      (red-lrange "l" 0 10))
    (check true              (red-ltrim "l" 2 3))
    (check null              (red-lrange "l" 0 10))
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
    #+nil (red-slaveof)))

(deftest sort()
  (with-test-db
    (check true                    (red-rpush "numbers" "1"))
    (check true                    (red-rpush "numbers" "2"))
    (check true                    (red-rpush "numbers" "3"))
    (check true                    (red-set "object_1" "o1"))
    (check true                    (red-set "object_2" "o2"))
    (check true                    (red-set "object_3" "o3"))
    (check true                    (red-set "weight_1" "47"))  
    (check true                    (red-set "weight_2" "13"))  
    (check true                    (red-set "weight_3" "32"))
    (check equal '("1" "2" "3")    (red-sort "numbers"))
    (check equal '("3" "2" "1")    (red-sort "numbers" :desc t))
    (check equal '("2" "3" "1")    (red-sort "numbers" :by "weight_*"))
    (check equal '("o2" "o3" "o1") (red-sort "numbers" :by "weight_*"
                                             :get "object_*"))
    (check equal '("o1" "o3" "o2") (red-sort "numbers" :by "weight_*"
                                             :get "object_*"
                                             :desc t))))


(defun run-tests (&key debug)
  (terpri)
  (princ "Runnning CL-REDIS tests... ")
  (redis-connect :debug debug)
  (princ (if (every (lambda (rez)
                      (and-it (mklist rez)
                              (every #'true it)))
                    (run-test #+nil tell
                              expect
                              commands
                              sort))
             "OK"
             (format nil "some tests failed. See log file for details: ~a"
                     *logg-out*)))
  (terpri)
  (terpri))
      

;;; end