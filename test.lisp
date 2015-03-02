;;; CL-REDIS testsuite package definition
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :cl-user)

(defpackage #:cl-redis-test
  (:use :common-lisp #:rutil #:redis #:should-test)
  (:export #:run-tests))

(in-package #:cl-redis-test)
(named-readtables:in-readtable rutils-readtable)


(defun run-tests ()
  (st:test :package '#:cl-redis-test))


;;; utils

(defun find-s (str seq)
  (find str seq :test #'string=))

(defun null-diff (set1 set2)
  (null (set-exclusive-or set1 set2 :test #'equal)))

(defmacro with-test-db (&body body)
  `(with-connection ()
     (red-select 15)
     (red-flushdb)
     (unwind-protect (progn ,@body)
       (red-flushdb))))

(defun expect-from-str (expected input)
  (let ((server (usocket:socket-listen usocket:*wildcard-host* 63799
                                       :reuseaddress t)))
    (unwind-protect
         (let ((bt:*default-special-bindings*
                (append `((*connection* . ,*connection*)
                          (*trace-output* . ,*trace-output*))
                        bt:*default-special-bindings*)))
           (bt:make-thread
            (lambda ()
              (let ((client
                     (flex:make-flexi-stream
                      (usocket:socket-stream (usocket:socket-accept
                                              server :element-type
                                              'flex:octet))
                      :external-format redis::+utf8+
                      :element-type 'flex:octet)))
                (mapcar #`((write-sequence (if (stringp %)
                                               (flex:string-to-octets
                                                % :external-format redis::+utf8+)
                                               (map 'vector #'code-char %))
                                           client)
                           (write-byte (char-code #\Return) client)
                           (write-byte (char-code #\Linefeed) client))
                        (mklist input))
                (finish-output client))))
           (with-connection (:port 63799)
             (expect expected)))
      (usocket:socket-close server))))


;;; tests

(deftest expect ()
  (should be string= "OK"
          (expect-from-str :status "+OK"))
  (should be string= "10$"
          (expect-from-str :inline "+10$"))
  (should be null
          (expect-from-str :boolean "+0$"))
  (should be = 10
          (expect-from-str :integer "+10"))
  (should be = 10.0d0
          (expect-from-str :float '("+4" "10.0")))
  (should be string= "abc"
          (expect-from-str :bulk '("+3" "abc")))
  (should be string= ""
          (expect-from-str :bulk '("+0" "")))
  (should be equal '("a" nil)
          (expect-from-str :multi '("*2" "$1" "a" "$-1")))
  ;; undocumented case for $0, let's be on the safe side
  (should be equal '("a" "")
          (expect-from-str :anything '("*2" "$1" "a" "$0" "")))
  (should be equal '("a" "b" "c")
          (expect-from-str :list '("+5" "a b c")))
  (should be equal '("OK" ("a"))
          (expect-from-str :queued '("*2" "+OK" "*1" "$1" "a")))
  (should be equal '(("subscribe" "chan1" "1") ("subscribe" "chan2" "2"))
          (expect-from-str :pubsub '("*3" "$9" "subscribe"
                                     "$5" "chan1" ":1"
                                     "*3" "$9" "subscribe"
                                     "$5" "chan2" ":2")))
  (should be equalp #(1 2 3)
          (expect-from-str :bytes '("*3" #(1 2 3)))))

(deftest tell ()
  (with-connection ()
    (let ((*echo-p* t)
          (*echo-stream* (make-string-output-stream)))
      (should be string=
              (fmt " > *3~% > $4~% > HGET~% > $2~% > h1~% > $5~% > f1~%~~~%")
              (progn (tell 'hget "h1" (format nil "~A~%~B" "f1" "~"))
                     (get-output-stream-string *echo-stream*))))))

(deftest connection ()
  (should be null
          (disconnect))
  (should be redis::connection-open-p
          (connect))
  (should signal redis-error
          (connect))
  (should be null
          (disconnect))
  (with-connection ()
    (should be string= "Hello World!"
            (red-echo "Hello World!"))
    (should signal redis-error-reply
            (red-auth "pass")))
  ;; QUIT - futile
  )

(deftest *-commands ()
  (with-test-db
    (should be string= "OK"
            (red-set "mykey" "Hello"))
    (should be true
            (red-expire "mykey" 10))
    (should be = 10
            (red-ttl "mykey"))
    (should be true
            (red-pexpire "mykey" 10000))
    (should be < 10001
            (red-pttl "mykey"))
    (should be true
            (red-persist "mykey"))
    (should be = -1
            (red-ttl "mykey"))
    (should be true
            (red-exists "mykey"))
    (should be true
            (red-expireat "mykey" (- (get-universal-time) 2208988800)))
    (should be null
            (red-exists "mykey"))
    (should be string= "OK"
            (red-set "mykey" "Hello"))
    (should be true
            (red-pexpireat "mykey" (* 1000 (- (get-universal-time) 2208988800))))
    (should be null
            (red-exists "mykey"))
    (should be = 1
            (red-lpush "mylist" "Hello World"))
    (should be = 1
            (red-object-refcount "mylist"))
    (should be string= "ziplist"
            (red-object-encoding "mylist"))
    (should be = -1
            (red-object-idletime "mykey"))
    (should be string= "OK"
            (red-set "foo" 1000))
    (should be string= "int"
            (red-object-encoding "foo"))
    (should be = 7
            (red-append "foo" "bar"))
    (should be string= "1000bar"
            (red-get "foo"))
    (should be string= "raw"
            (red-object-encoding "foo"))
    (should be true
            (red-move "foo" 14))
    (should be null
            (red-move "foo" 14))
    (should be true
            (red-select 14))
    (should be true
            (red-del "foo"))
    (should be true
            (red-select 15))
    (should be string= "OK"
            (red-set "mykey" 10))
    (should be equalp #(0 192 10 6 0 248 114 63 197 251 251 95 40)
            (red-dump "mykey"))
    (should be true
            (red-del "mykey"))
    (should be string= "OK"
            (red-restore "mykey" 0 #(0 192 10 6 0 248 114 63 197 251 251 95 40)))
    (should be string= "string"
            (red-type "mykey"))
    (should be string= "10"
            (red-get "mykey"))))

(deftest sort-command ()
  (with-test-db
    (should be true
            (red-rpush "numbers" "1"))
    (should be true
            (red-rpush "числа" "1"))
    (should be true
            (red-rpush "numbers" "2"))
    (should be true
            (red-rpush "числа" "2"))
    (should be true
            (red-rpush "numbers" "3"))
    (should be true
            (red-rpush "числа" "3"))
    (should be true
            (red-set "object_1" "o1"))
    (should be true
            (red-set "об'єкт_1" "о1"))
    (should be true
            (red-set "object_2" "o2"))
    (should be true
            (red-set "об'єкт_2" "о2"))
    (should be true
            (red-set "object_3" "o3"))
    (should be true
            (red-set "об'єкт_3" "о3"))
    (should be true
            (red-set "weight_1" "47"))
    (should be true
            (red-set "вага_1" "47"))
    (should be true
            (red-set "weight_2" "13"))
    (should be true
            (red-set "вага_2" "13"))
    (should be true
            (red-set "weight_3" "32"))
    (should be true
            (red-set "вага_3" "32"))
    (should be equal '("1" "2" "3")
            (red-sort "numbers"))
    (should be = 3
            (red-sort "numbers" :store "stored"))
    (should be equal '("1" "2" "3")
            (red-lrange "stored" 0 -1))
    (should be equal '("1" "2" "3")
            (red-sort "числа"))
    (should be equal '("2" "3")
            (red-sort "numbers" :start 1 :end 2))
    (should be equal '("2" "3")
            (red-sort "числа" :start 1 :end 2))
    (should be equal '("3" "2" "1")
            (red-sort "numbers" :desc t))
    (should be equal '("2" "1")
            (red-sort "numbers" :desc t :start 1 :end 2))
    (should be equal '("3" "2" "1")
            (red-sort "числа" :desc t))
    (should be equal '("2" "1")
            (red-sort "числа"
                      :desc t :start 1 :end 2))
    (should be equal '("2" "3" "1")
            (red-sort "numbers" :by "weight_*"))
    (should be equal '("2" "3" "1")
            (red-sort "числа"
                      :by "вага_*"))
    (should be equal '("o2" "o3" "o1")
            (red-sort "numbers" :by "weight_*" :get "object_*"))
    (should be equal '("о2" "о3" "о1")
            (red-sort "числа"
                      :by "вага_*"
                      :get "об'єкт_*"))
    (should be equal '("o1" "o3" "o2")
            (red-sort "numbers" :by "weight_*" :get "object_*" :desc t))
    (should be equal '("о1" "о3" "о2")
            (red-sort "числа"
                      :by "вага_*"
                      :get "об'єкт_*"
                      :desc t))))

(deftest str-commands ()
  (with-test-db
    (should be true
            (red-set "y" "1"))
    (should be true
            (red-set "ігрек" "1"))
    (should be string= "1"
            (red-getset "y" "2"))
    (should be string= "1"
            (red-getset "ігрек" "2"))
    (should be string= "2"
            (red-get "y"))
    (should be string= "2"
            (red-get "ігрек"))
    (should be true
            (red-set "z" "3"))
    (should be true
            (red-set "зед" "3"))
    (should be equal '("2" "3")
            (red-mget "y" "z"))
    (should be equal '("2" "3")
            (red-mget "ігрек" "зед"))
    (should be equal '("2" nil)
            (red-mget "y" "a"))
    (should be equal '("2" nil)
            (red-mget "ігрек" "а"))
    (should be null
            (red-setnx "z" "3"))
    (should be null
            (red-setnx "зед" "3"))
    (should be true
            (red-setnx "u" "3"))
    (should be true
            (red-setnx "ю" "3"))
    (should be string= "OK"
            (red-setex "v" 1 0))
    (should be null
            (progn (sleep 2) (red-get "v")))
    (should be = 4
            (red-incr "u"))
    (should be = 4
            (red-incr "ю"))
    (should be = 6
            (red-incrby "u" 2))
    (should be = 6
            (red-incrby "ю" 2))
    (should be = 5.0
            (red-incrbyfloat "u" -1.0))
    (should be = 5
            (red-decr "ю"))
    (should be = 3
            (red-decrby "ю" 2))
    (should be true
            (red-exists "u"))
    (should be true
            (red-exists "ю"))
    (should be null
            (red-exists "v"))
    (should be null
            (red-exists "ві"))
    (should be true
            (red-del "u"))
    (should be true
            (red-del "ю"))
    (should be null
            (red-exists "u"))
    (should be null
            (red-exists "ю"))
    (should be string= "none"
            (red-type "u"))
    (should be string= "none"
            (red-type "ю"))
    (should be string= "string"
            (red-type "z"))
    (should be string= "string"
            (red-type "зед"))
    (should be null-diff '("y" "ігрек"
                           "z" "зед")
            (red-keys "*"))
    (should be string= "OK"
            (red-rename "z" "c"))
    (should be string= "OK"
            (red-rename "зед" "це"))
    (should signal redis-error-reply
            (red-rename "z" "d"))
    (should be string= "3"
            (red-get "c"))
    (should be string= "3"
            (red-get "це"))
    (should be null
            (red-renamenx "y" "c"))
    (should be null
            (red-renamenx "ігрек" "це"))
    (should be true
            (red-renamenx "y" "b"))
    (should be true
            (red-renamenx "ігрек" "бе"))
    (should signal redis-error-reply
            (red-renamenx "b" "b"))
    (should signal redis-error-reply
            (red-renamenx "бе" "бе"))
    (should be = 4
            (red-dbsize))
    (should be true
            (red-expire "b" 1))
    (should be true
            (red-expire "бе" 1))
    (should be null
            (progn (sleep 2) (red-get "b")))
    (should be null
            (red-get "бе"))
    (should be null
            (red-expire "b" 1))
    (should be find-s '("c" "це")
            (red-randomkey))
    (should be true
            (red-expire "c" 600))
    (should be true
            (red-expire "це" 600))
    (should be < 601
            (red-ttl "c"))
    (should be < 601
            (red-ttl "це"))
    (should be true
            (red-mset "k1" "v1" "k2" "v2"))
    (should be true
            (red-mset "ка1" "ве1"
                      "ка2" "ве2"))
    (should be null
            (red-msetnx "k1" "w1" "k3" "v3"))
    (should be null
            (red-msetnx "ка1"
                        "дубльве1"
                        "ка3"
                        "ве3"))
    (should be null
            (red-exists "k3"))
    (should be null
            (red-exists "ка3"))
    (should be true
            (red-msetnx "k4" "v4" "k5" "v5"))
    (should be true
            (red-msetnx "ка4" "ве4"
                        "ка5" "ве5"))
    (should be equal '("v1" "v2" "v4" "v5")
            (red-mget "k1" "k2" "k4" "k5"))
    (should be equal '("ве1" "ве2"
                       "ве4" "ве5")
            (red-mget "ка1" "ка2"
                      "ка4" "ка5"))
    (should be true
            (red-mset "k1" "w1" "k2" "v2"))
    (should be true
            (red-mset "ка1"
                      "дубльве1"
                      "ка2"
                      "ве2"))
    (should be equal "w1"
            (red-get "k1"))
    (should be equal "дубльве1"
            (red-get "ка1"))
    (should be null
            (red-exists "mykey"))
    (should be = 6
            (red-append "mykey" "Hello "))
    (should be = 11
            (red-append "mykey" "World"))
    (should be string= "Hello World"
            (red-get "mykey"))
    (should be string= "OK"
            (red-set "s" "This is a string"))
    (should be string= "This"
            (red-substr "s" 0 3))
    (should be string= "ing"
            (red-substr "s" -3 -1))
    (should be string= "This is a string"
            (red-substr "s" 0 -1))
    (should be string= " string"
            (red-substr "s" 9 100000))
    (should be string= "OK"
            (red-set "mykey" "This is a string"))
    (should be string= "This"
            (red-getrange "mykey" 0 3))
    (should be string= "ing"
            (red-getrange "mykey" -3 -1))
    (should be string= "This is a string"
            (red-getrange "mykey" 0 -1))
    (should be = 16
            (red-setrange "mykey" 10 "Redis"))
    (should be string= "This is a Redisg"
            (red-get "mykey"))
    (should be = 11
            (red-setrange "key2" 6 "Redis"))
    (should be string= "      Redis"
            (red-get "key2"))
    (should be string= "Redisg"
            (red-getrange "mykey" 10 100))
    (should be zerop
            (red-setbit "mykey" 7 1))
    (should be = 1
            (red-getbit "mykey" 7))
    (should be zerop
            (red-getbit "mykey" 10000))
    (should be = 55
            (red-bitcount "mykey"))
    (should be = 16
            (red-bitop "NOT" "mykey2" "mykey"))
    (should be string= "Uhis is a Redisg"
            (red-get "mykey"))
    (should be = 16
            (red-strlen "mykey"))
    (should be zerop
            (red-strlen "nonex key"))
    (should be string= "OK"
            (red:set "mykey" (apply #'fmt "~C~C~C"
                                    (mapcar #'code-char '(#xf0 #xff #x00)))))
    (should be = 2
            (red:bitpos "mykey" 0))
    (should be string= "OK"
            (red:set "mykey" (apply #'fmt "~C~C~C"
                                    (mapcar #'code-char '(#x00 #xff #xf0)))))
    (should be = 8
            (red:bitpos "mykey" 1 0))
    (should be = 16
            (red:bitpos "mykey" 1 2))
    (should be string= "OK"
            (red:set "mykey" (apply #'fmt "~C~C~C"
                                    (mapcar #'code-char '(#x00 #x00 #x00)))))
    (should be = -1
            (red:bitpos "mykey" 1))))

(deftest l-commands ()
  (with-test-db
    (should be = 1
            (red-rpush "l" "1"))
    (should be = 1
            (red-rpush "эл" "1"))
    (should be = 2
            (red-rpush "l" "1"))
    (should be = 2
            (red-rpush "эл" "1"))
    (should be = 3
            (red-rpush "l" "1"))
    (should be = 3
            (red-rpush "эл" "1"))
    (should be = 3
            (red-lrem "l" 0 "1"))
    (should be = 3
            (red-lrem "эл" 0 "1"))
    (should be = 0
            (red-lrem "l" 0 "a"))
    (should be = 0
            (red-lrem "эл" 0 "а"))
    (should be true
            (red-lpush "l" "1"))
    (should be true
            (red-lpush "эл" "1"))
    (should be true
            (red-lpush "l" "0"))
    (should be true
            (red-lpush "эл" "0"))
    (should be = 2
            (red-llen "l"))
    (should be = 2
            (red-llen "эл"))
    (should be equal '("0")
            (red-lrange "l" 0 0))
    (should be equal '("0")
            (red-lrange "эл" 0 0))
    (should be equal '("0" "1")
            (red-lrange "l" 0 -1))
    (should be equal '("0" "1")
            (red-lrange "l" 0 2))
    (should be equal '("0" "1")
            (red-lrange "l" 0 10))
    (should be null
            (red-lrange "l" 2 1))
    (should be null
            (red-lrange "l" 2 3))
    (should be string= "0"
            (red-lindex "l" 0))
    (should be string= "0"
            (red-lindex "эл" 0))
    (should be true
            (red-lset "l" 0 "a"))
    (should be true
            (red-lset "эл" 0 "а"))
    (should be equal '("a" "1")
            (red-lrange "l" 0 10))
    (should be equal '("а" "1")
            (red-lrange "эл" 0 10))
    (should be true
            (red-ltrim "l" 0 0))
    (should be true
            (red-ltrim "эл" 0 0))
    (should be equal '("a")
            (red-lrange "l" 0 10))
    (should be equal '("а")
            (red-lrange "эл" 0 10))
    (should be true
            (red-ltrim "l" 2 3))
    (should be true
            (red-ltrim "эл" 2 3))
    (should be null
            (red-lrange "l" 0 10))
    (should be null
            (red-lrange "эл" 0 10))
    (should be true
            (red-lpush "l" "2"))
    (should be true
            (red-lpush "эл" "2"))
    (should be true
            (red-rpush "l" "3"))
    (should be true
            (red-rpush "эл" "3"))
    (should be true
            (red-rpush "l" "4"))
    (should be true
            (red-rpush "эл" "4"))
    (should be true
            (red-rpush "эл" "5"))
    (should be true
            (red-rpush "эл" "6"))
    (should be string= "2"
            (red-lpop "l"))
    (should be string= "2"
            (red-lpop "эл"))
    (should be string= "4"
            (red-rpop "l"))
    (should be string= "3"
            (red-rpop "l"))
    (should be string= "6"
            (red-rpop "эл"))
    (should be null
            (red-blpop "l" 1))
    (should be true
            (red-rpush "l" "5"))
    (should be equal '("l" "5")
            (red-blpop "l" 1))
    (should be equal '("эл" "3")
            (red-blpop "эл" 1))
    (should be true
            (red-rpush "l" "0"))
    (should be true
            (red-rpush "l" "1"))
    (should be true
            (red-rpush "l" "2"))
    (should be equal '("0" "1" "2")
            (red-lrange "l" 0 -1))
    (should be string= "4"
            (red-lpop "эл"))
    (should be string= "5"
            (red-lpop "эл"))
    (should be null
            (red-lrange "эл" 0 -1))
    (should be string= "2"
            (red-rpoplpush "l" "эл"))
    (should be string= "1"
            (red-rpoplpush "l" "l"))
    (should be string= "0"
            (red-brpoplpush "l" "l" 0))
    (should be null
            (progn (sleep 2) (red-brpoplpush "abc" "l" 1)))
    (should be equal '("2")
            (red-lrange "эл" 0 1))
    (should be equal '("0" "1")
            (red-lrange "l" 0 2))
    (should be string= "1"
            (red-rpop "l"))
    (should signal redis-error-reply
            (red-get "l"))
    (should signal redis-error-reply
            (red-get "эл"))
    (should be = 1
            (red-lpush "mylist" "World"))
    (should be = 2
            (red-lpushx "mylist" "Hello"))
    (should be = 0
            (red-lpushx "myotherlist" "Hello"))
    (should be equal '("Hello" "World")
            (red-lrange "mylist" 0 -1))
    (should be null
            (red-lrange "myotherlist" 0 -1))
    (should be = 1
            (red-rpush "mylist2" "Hello"))
    (should be = 2
            (red-rpushx "mylist2" "World"))
    (should be zerop
            (red-rpushx "myotherlist" "Hello"))
    (should be equal '("Hello" "World")
            (red-lrange "mylist2" 0 -1))
    (should be null
            (red-lrange "myotherlist" 0 -1))
    (should be = 3
            (red-linsert "mylist2" :before "World" "There"))
    (should be equal '("Hello" "There" "World")
            (red-lrange "mylist2" 0 -1))
    (should be = 4
            (red-linsert "mylist2" :after "World" "!"))
    (should be equal '("Hello" "There" "World" "!")
            (red-lrange "mylist2" 0 -1))
    (should signal simple-error
            (red-linsert "mylist2" :inside "World" "1"))
    (should be = -1
            (red-linsert "mylist2" :before "W" "1"))))

(deftest s-commands ()
  (with-test-db
    (should be = 1
            (red-sadd "s" "1"))
    (should be = 1
            (red-sadd "э" "1"))
    (should be = 0
            (red-sadd "s" "1"))
    (should be = 0
            (red-sadd "э" "1"))
    (should be = 1
            (red-sadd "s" "2"))
    (should be = 1
            (red-sadd "э" "2"))
    (should be find-s '("2" "1")
            (red-srandmember "s"))
    (should be equal '("1" "2")
            (red-srandmember "s" 2))
    (should be find-s '("2" "1")
            (red-spop "s"))
    (should be find-s '("2" "1")
            (red-spop "э"))
    (should be true
            (or (red-sadd "s" "2") (red-sadd "s" "1")))
    (should be true
            (or (red-sadd "э" "2") (red-sadd "э" "1")))
    (should be true
            (red-srem "s" "1"))
    (should be true
            (red-srem "э" "1"))
    (should be string= "2"
            (red-spop "s"))
    (should be string= "2"
            (red-spop "э"))
    (should be null
            (red-spop "s"))
    (should be null
            (red-spop "э"))
    (should be true
            (red-sadd "s" "2"))
    (should be true
            (red-sadd "э" "2"))
    (should be true
            (red-sismember "s" "2"))
    (should be true
            (red-sismember "э" "2"))
    (should be true
            (red-sadd "s" "1"))
    (should be true
            (red-sadd "э" "1"))
    (should be true
            (red-smove "s" "s2" "1"))
    (should be true
            (red-smove "э" "э2" "1"))
    (should be true
            (red-sismember "s2" "1"))
    (should be true
            (red-sismember "э2" "1"))
    (should be null
            (red-smove "s" "s2" "3"))
    (should be null
            (red-smove "э" "э2" "3"))
    (should be null
            (red-sismember "s2" "3"))
    (should be null
            (red-sismember "э2" "3"))
    (should be true
            (red-sadd "s" "1"))
    (should be true
            (red-sadd "э" "1"))
    (should be true
            (red-smove "s" "s2" "1"))
    (should be true
            (red-smove "э" "э2" "1"))
    (should be = 1
            (red-scard "s"))
    (should be = 1
            (red-scard "э"))
    (should be null
            (red-sinter "s" "s2"))
    (should be null
            (red-sinter "э" "э2"))
    (should be true
            (red-sadd "s" "1"))
    (should be true
            (red-sadd "э" "1"))
    (should be equal '("1")
            (red-sinter "s" "s2"))
    (should be equal '("1")
            (red-sinter "э" "э2"))
    (should be true
            (red-sinterstore "s3" "s" "s2"))
    (should be true
            (red-sinterstore "э3" "э" "э2"))
    (should be equal '("1")
            (red-smembers "s3"))
    (should be equal '("1")
            (red-smembers "э3"))
    (should be null-diff '("1" "2")
            (red-sunion "s" "s2"))
    (should be null-diff '("1" "2")
            (red-sunion "э" "э2"))
    (should be true
            (red-sunionstore "s4" "s" "s2"))
    (should be true
            (red-sunionstore "э4" "э" "э2"))
    (should be null-diff '("1" "2")
            (red-smembers "s4"))
    (should be equal '("1" "2")
            (red-smembers "э4"))
    (should be equal '("2")
            (red-sdiff "s4" "s3"))
    (should be equal '("2")
            (red-sdiff "э4" "э3"))
    (should be true
            (red-sdiffstore "s5" "s4" "s3"))
    (should be true
            (red-sdiffstore "э5" "э4" "э3"))
    (should be equal '("2")
            (red-smembers "s5"))
    (should be equal '("2")
            (red-smembers "э5"))))

(deftest z-commands ()
  (with-test-db
    (should be true
            (red-zadd "set" 1 "e1"))
    (should be true
            (red-zadd "множина"
                      1 "елемент1"))
    (should be true
            (red-zadd "set" 2 "e2"))
    (should be true
            (red-zadd "множина"
                      2 "елемент2"))
    (should be true
            (red-zadd "set" 3 "e3"))
    (should be true
            (red-zadd "множина"
                      3 "елемент3"))
    (should be true
            (red-zrem "set" "e2"))
    (should be true
            (red-zrem "множина"
                      "елемент2"))
    (should be zerop
            (red-zrem "set" "e2"))
    (should be zerop
            (red-zrem "множина"
                      "елемент2"))
    (should be true
            (red-zadd "set" 10 "e2"))
    (should be true
            (red-zadd "множина"
                      10 "елемент2"))
    (should be true
            (red-zadd "set" 4 "e4"))
    (should be true
            (red-zadd "множина"
                      4 "елемент4"))
    (should be true
            (red-zadd "set" 5 "e5"))
    (should be true
            (red-zadd "множина"
                      5 "елемент5"))
    (should be = 5
            (red-zcard "set"))
    (should be = 10.0d0
            (red-zscore "set" "e2"))
    (should be = 4
            (red-zrank "set" "e2"))
    (should be zerop
            (red-zrevrank "set" "e2"))
    (should be equal '("e3" "e4" "e5")
            (red-zrange "set" 1 3))
    (should be equal '("елемент3"
                       "елемент4"
                       "елемент5")
            (red-zrange "множина" 1 3))
    (should be equal '("e4" "e3" "e1")
            (red-zrevrange "set" 2 4))
    (should be equal '("елемент4" "4"
                       "елемент3" "3"
                       "елемент1" "1")
            (red-zrevrange "множина"
                           2 4 :withscores))
    (should be equal '("e5" "e2")
            (red-zrangebyscore "set" 5 10))
    (should be equal '("елемент1"
                       "елемент3"
                       "елемент4"
                       "елемент5"
                       "елемент2")
            (red-zrangebyscore "множина"
                               "-inf" "+inf"))
    (should be equal '("e5" "5" "e4" "4")
            (red-zrevrangebyscore "set" "(10" 4 :withscores t))
    (should be equal '("елемент5" "5")
            (red-zrevrangebyscore
             "множина"
             10 5 :withscores t :limit '(1 . 1)))
    (should be = 3
            (red-zremrangebyscore "set" 2 7))
    (should be = 3
            (red-zremrangebyrank
             "множина" 0 2))
    (should be equal '("e1" "e2")
            (red-zrange "set" 0 -1))
    (should be equal '("елемент5"
                       "елемент2")
            (red-zrange "множина" 0 -1))
    (should be = 7
            (red:zadd "myzset" 0 "a" 0 "b" 0 "c" 0 "d" 0 "e" 0 "f" 0 "g"))
    (should be equal '("c" "b" "a")
            (red:zrevrangebylex "myzset" "[c" "-"))
    (should be equal '("b" "a")
            (red:zrevrangebylex "myzset" "(c" "-"))
    (should be equal '("f" "e" "d" "c" "b")
            (red:zrevrangebylex "myzset" "(g" "[aaa"))
    (should be = 1
            (red:zadd "myzset" 0 "aaaa" 0 "b" 0 "c" 0 "d" 0 "e"))
    (should be = 5
            (red:zadd "myzset" 0 "foo" 0 "zap" 0 "zip" 0 "ALPHA" 0 "alpha"))
    (should be equal '("ALPHA" "a" "aaaa" "alpha" "b" "c" "d" "e" "f" "foo" "g"
                       "zap" "zip")
            (red:zrange "myzset" 0 -1))
    (should be = 8
            (red:zremrangebylex "myzset" "[alpha" "[omega"))
    (should be equal '("ALPHA" "a" "aaaa" "zap" "zip")
            (red:zrange "myzset" 0 -1))
    (should be = 4
            (red-zunionstore
             "s1" 2 '("set" "множина")))
    (should be zerop
            (red-zinterstore
             "s2" 2 '("set" "множина")
             :weights '(1 2) :aggregate :min))
    (should be = 2
            (red-zinterstore "s3" 2 '("set" "s1") :aggregate :sum))
    (should be true
            (red-zadd "myzset" 1 "one"))
    (should be true
            (red-zadd "myzset" 1 "two"))
    (should be true
            (red-zadd "myzset" 1 "three"))
    (should be = 8
            (red-zcount "myzset" "-inf" "+inf"))
    (should be zerop
            (red-zcount "myzset" "(1" "3"))
    (should be = 4
            (red:zadd "myzset" 0 "a" 0 "b" 0 "c" 0 "d" 0 "e"))
    (should be = 2
            (red:zadd "myzset" 0 "f" 0 "g"))
    (should be = 14
            (red:zlexcount "myzset" "-" "+"))
    (should be = 5
            (red:zlexcount "myzset" "[b" "[f"))
    (should be = 1
            (red-zincrby "myzset" 3 "two"))))

(deftest h-commands ()
  (with-test-db
    (should be true
            (red-hset "h1" "f1" "a"))
    (should be true
            (red-hset "h1" "f2" "b"))
    (should be null
            (red-hset "h1" "f1" "c"))
    (should be string= "c"
            (red-hget "h1" "f1"))
    (should be equal '("c" "b")
            (red-hmget "h1" "f1" "f2"))
    (should be string= "OK"
            (red-hmset "h1" "f1" "1" "f2" "2"))
    (should be = 3
            (red-hincrby "h1" "f2" "1"))
    (should be zerop
            (red-hincrby "h1" "f1" "-1"))
    (should be = 0.1d0
            (red-hincrbyfloat "h1" "f1" "0.1"))
    (should be true
            (red-hexists "h1" "f1"))
    (should be null
            (red-hexists "h1" "f3"))
    (should be true
            (red-hdel "h1" "f1"))
    (should be null
            (red-hdel "h1" "f3"))
    (should be = 1
            (red-hlen "h1"))
    (should be equal '("f2")
            (red-hkeys "h1"))
    (should be equal '("3")
            (red-hvals "h1"))
    (should be equal '("f2" "3")
            (red-hgetall "h1"))
    (should be true
            (red-hsetnx "myhash" "field" "Hello"))
    (should be null
            (red-hsetnx "myhash" "field" "World"))
    (should be string= "Hello"
            (red-hget "myhash" "field"))
    #+v.3.2.0
    (should be = 5
            (red-hstrlen "myhash" "field"))))

(deftest pf-commands ()
  (with-test-db
    (should be = 1
            (red:pfadd "hll" "a" "b" "c" "d" "e" "f" "g"))
    (should be = 7
            (red:pfcount "hll"))
    (should be = 1
            (red:pfadd "hll1" "foo" "bar" "zap" "a"))
    (should be = 1
            (red:pfadd "hll2" "a" "b" "c" "foo"))
    (should be string= "OK"
            (red:pfmerge "hll3" "hll1" "hll2"))
    (should be = 6
            (red:pfcount "hll3"))))

(deftest transaction-commands ()
  (with-test-db
    (should be string= "OK"
            (red-multi))
    (should be string= "QUEUED"
            (red-incr "foo"))
    (should be string= "QUEUED"
            (red-incr "bar"))
    (should be string= "QUEUED"
            (red-incr "bar"))
    (should be equal '(1 1 2)
            (red-exec))
    (should be string= "OK"
            (red-multi))
    (should be string= "QUEUED"
            (red-set "a" "abc"))
    (should be string= "QUEUED"
            (red-lpop "a"))
    (should signal redis-error-reply
            (red-exec))
    (should be true
            (red-set "foo" "1"))
    (should be string= "OK"
            (red-multi))
    (should be string= "QUEUED"
            (red-incr "foo"))
    (should be string= "OK"
            (red-discard))
    (should be string= "1"
            (red-get "foo"))
    (should be string= "OK"
            (red-watch "abc"))
    (should be string= "OK"
            (red-unwatch))))

(deftest pubsub-commands ()
  (with-test-db
    (should be equal '(("subscribe" "foo" "1") ("subscribe" "bar" "2"))
            (red-subscribe "foo" "bar"))
    (should be equal '("message" "foo" "test")
            (progn (bt:make-thread (lambda ()
                                     (let ((*echo-p* nil))
                                       (sleep 1)
                                       (with-connection ()
                                         (red-publish "foo" "test")))))
                   (expect :multi)))
    (should be equal '(("unsubscribe" "bar" "1"))
            (red-unsubscribe "bar"))
    (should be equal '(("unsubscribe" "foo" "0"))
            (red-unsubscribe))
    (should be equal '(("psubscribe" "news.*" "1"))
            (red-psubscribe "news.*"))
    (should be equal '("pmessage" "news.*" "news.1" "puf")
            (progn (bt:make-thread (lambda ()
                                     (let ((*echo-p* nil))
                                       (sleep 1)
                                       (with-connection ()
                                       (red-publish "news.1" "puf")))))
                   (expect :multi)))
    (should be equal '(("punsubscribe" "news.*" "0"))
            (red-punsubscribe))
    (should be zerop
            (red-publish "test" "test"))))

(deftest pipelining-commands ()
  (with-test-db
    (should be equal '("PONG" 0)
            (with-pipelining
              (red-ping)
              (red-dbsize)))
    (handler-bind ((warning #`(invoke-restart (find-restart 'muffle-warning %))))
      (should be equal '("PONG" "PONG")
              (with-pipelining
                (red-ping)
                (with-pipelining
                  (red-ping)))))))

(deftest server-commands ()
  (with-test-db
    (should be true
            (red:save))
    (should be true
            (red:bgsave))
    (should be integerp
            (red:lastsave))
    (should be string= "redis_version"
            (let ((info (red:info)))
              (if (char= #\# (char info 0))
                  (sub info 10 23)
                  (sub info 0 13))))
    (should be string= "OK"
            (red:slaveof "no" "one"))
    (should be string= "save"
            (first (red:config-get "save")))
    (should be string= "OK"
            (handler-case (red:config-rewrite)
              (redis-error (e)
                (if (string= "ERR The server is running without a config file"
                             (redis-error-message e))
                    "OK"
                    (error e)))))
    (should be string= "OK"
            (red:config-set "timeout" 200))
    (should be string= "OK"
            (red:config-resetstat))
    ;; the next commands have unpredicatble results - just let be, that they run
    (should be null
            (red:slowlog :get 10))
    (should be true
            (red:slowlog :len))
    (should be true
            (red:slowlog :reset))
    (should be plusp
            (reduce '+ (mapcar 'parse-integer (red:time))))
    (should be string= "OK"
            (red:client-setname "test"))
    (should be string= "test"
            (red:client-getname))
    (should be string= "OK"
            (red:client-pause 1))
    ;; SYNC, BGREWRITEAOF - may be too long
    ;; SHUTDOWN - futile
    ;; FLUSHALL - don't do this at home
    ;; MIGRATE - don't have other host
    ;; CLUSRE-SLOTS - need cluster
    ;; COMMAND, COMMAND-COUNT, COMMAND-GETKEYS, COMMAND-INFO - ???
    ))


(deftest scripting-commands ()
  (let ((sha1 "080c414e64bca1184bc4f6220a19c4d495ac896d"))
    (with-test-db
      (should be = 10
              (red:eval "return 10" 0))
      (should be equal '(1 2 (3 "Hello World!"))
              (red:eval "return {1,2,{3,'Hello World!'}}" 0))
      (should be string= sha1
              (red:script-load "return 10"))
      (should be = 10
              (red:evalsha sha1 0))
      (should be equal '(1 0)
              (red:script-exists sha1 "ffffffffffffffffffffffffffffffffffffffff"))
      (should signal redis-error-reply
              (red:script-kill))
      (progn
        (bt:make-thread #`(with-connection ()
                            (ignore-errors
                              (red:evalsha (red:script-load
                                            "while true do print(1) end")
                                           0))))
        (sleep 1)  ; waiting for the previous thread to start
        (should be string= "OK"
                (red:script-kill)))
      (should be string= "OK"
              (red:script-flush))
      (should be equal '(0 0)
              (red:script-exists sha1 "ffffffffffffffffffffffffffffffffffffffff")))))

;;; end
