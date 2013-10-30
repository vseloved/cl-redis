;;; CL-REDIS commands
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions


(in-package #:redis)


;;; Connection handling

(def-cmd PING () :status
  "Ping server.")

(def-cmd QUIT () :end
  "Close the connection.")

(def-cmd AUTH (pass) :status
  "Simple password authentication if enabled.")

(def-cmd SELECT (index) :status
  "Select the DB having the specified index.")

(def-cmd ECHO (message) :bulk
  "Returns message.")


;;; Any key type commands

(def-cmd EXISTS (key) :boolean
  "Test if a key exists.")

(def-cmd DEL (key &rest keys) :integer
  "Delete a key.")

(def-cmd TYPE (key) :inline
  "Return the type of the value stored at key.")

(def-cmd KEYS (pattern) :multi
  "Return all the keys matching a given pattern.")

(def-cmd RANDOMKEY () :bulk
  "Return a random key from the key space.")

(def-cmd RENAME (oldname newname) :status
  "Rename the old key in the new one, destroing the newname key if it
already exists.")

(def-cmd RENAMENX (oldname newname) :boolean
  "Rename the old key in the new one, if the newname key does not
already exist.")

(def-cmd EXPIRE (key secs) :boolean
  "Set a time to live in SECS on KEY.")

(def-cmd EXPIREAT (key timestamp) :boolean
  "Set a timeout on KEY. After the timeout has expired, the key will
automatically be deleted.
EXPIREAT has the same effect and semantic as EXPIRE, but instead of
specifying the number of seconds representing the TTL, it takes
an absolute UNIX timestamp (seconds since January 1, 1970).
As in the case of EXPIRE command, if key is updated before the timeout has
expired, then the timeout is removed as if the PERSIST command was invoked
on KEY.")

(def-cmd PEXPIRE (key milliseconds) :boolean
  "Set a KEY's time to live in MILLISECONDS.")

(def-cmd PEXPIREAT (key milliseconds-timestamp) :boolean
  "Set the expiration for a KEY as a Unix timestamp specified in milliseconds.")

(def-cmd PERSIST (key) :boolean
  "Remove the existing timeout on KEY.")

(def-cmd TTL (key) :integer
  "Get the time to live in seconds of KEY.")

(def-cmd PTTL (key) :integer
  "Get the time to live in milliseconds of KEY.")

(def-cmd MOVE (key dbindex) :boolean
  "Move the key from the currently selected DB to the DB having as
index dbindex.")

(def-cmd SORT (key &rest args
                   &key  by     ; A pattern.
                         start
                         end
                         get    ; A pattern or a list of patterns.
                         desc   ; Should sort be descending?
                         alpha  ; Should sort be lexicographical?
                         store  ; Store result into key
                         ) (if store
                               :integer
                               :multi)
  "Sort a Set or a List accordingly to the specified parameters.")

(defmethod tell ((cmd (eql 'SORT)) &rest args)
  (ds-bind (key &key by get desc alpha start end store) args
    (assert (or (and start end)
                (and (null start) (null end))))
    (apply #'tell "SORT"
           (cl:append (list key)
                      (when by    `("BY" ,by))
                      (when get   `("GET" ,get))
                      (when desc  '("DESC"))
                      (when alpha '("ALPHA"))
                      (when start `("LIMIT" ,start ,end))
                      (when store `("STORE" ,store))))))

(def-cmd OBJECT-REFCOUNT (key) :integer
  "The OBJECT command allows to inspect the internals of Redis Objects
associated with keys. It is useful for debugging or to understand if your keys
are using the specially encoded data types to save space. Your application may
also use the information reported by the OBJECT command to implement application
level key eviction policies when using Redis as a Cache.

OBJECT REFCOUNT <key> returns the number of references of the value associated
with the specified key.")

(def-cmd OBJECT-ENCODING (key) :bulk
  "The OBJECT command allows to inspect the internals of Redis Objects
associated with keys. It is useful for debugging or to understand if your keys
are using the specially encoded data types to save space. Your application may
also use the information reported by the OBJECT command to implement application
level key eviction policies when using Redis as a Cache.

OBJECT ENCODING <key> returns the kind of internal representation used in order
to store the value associated with a key.")

(def-cmd OBJECT-IDLETIME (key) :integer
  "The OBJECT command allows to inspect the internals of Redis Objects
associated with keys. It is useful for debugging or to understand if your keys
are using the specially encoded data types to save space. Your application may
also use the information reported by the OBJECT command to implement application
level key eviction policies when using Redis as a Cache.

OBJECT IDLETIME <key> returns the number of seconds since the object stored
at the specified key is idle (not requested by read or write operations). While
the value is returned in seconds the actual resolution of this timer is 10
seconds, but may vary in future implementations.")

(def-cmd DUMP (key) :bytes
  "Return a serialized version of the value stored at the specified KEY.")

(def-cmd RESTORE (key ttl serialized-value) :status
  "Create a KEY using the provided SERIALIZED-VALUE,
previously obtained using DUMP.")

(defmethod tell ((cmd (eql 'RESTORE)) &rest args)
  (ds-bind (key ttl bytes) args
    (format-redis-line "*~A" (1+ (length args)))
    (dolist (arg (list cmd key ttl))
      (let ((arg (princ-to-string arg)))
        (format-redis-line "$~A" (flex:octet-length arg :external-format +utf8+))
        (format-redis-line "~A"  arg)))
    (let ((out (conn-stream *connection*)))
      (format-redis-line "$~A" (length bytes))
      (write-sequence bytes (flex:flexi-stream-stream out))
      (terpri out)
      (force-output out))))


(def-cmd MIGRATE (host port key destination-db timeout) :status
  "Atomically transfer a key from a Redis instance to another one.")


;;; String commands

(def-cmd SET (key value) :status
  "Set a key to a string value.")

(def-cmd GET (key) :bulk
  "Return the string value of the key.")

(def-cmd GETSET (key value) :bulk
  "Set a key to a string returning the old value of the key.")

(def-cmd MGET (&rest keys) :multi
  "Multi-get, return the strings values of the keys.")

(def-cmd SETNX (key value) :boolean
  "Set a key to a string value if the key does not exist.")

(def-cmd SETEX (key time value) :status
  "Set KEY to hold the string VALUE and set KEY to timeout after a given number
of seconds. This command is equivalent to executing the following commands:
    SET mykey value
    EXPIRE mykey seconds
SETEX is atomic, and can be reproduced by using the previous two commands inside
an MULTI/EXEC block. It is provided as a faster alternative to the given
sequence of operations, because this operation is very common when Redis is used
as a cache.
An error is returned when seconds is invalid.")

(def-cmd MSET (&rest key-value-plist) :status
  "Set multiple keys to multiple values in a single atomic operation.")

(def-cmd MSETNX (&rest key-value-plist) :boolean
  "Set multiple keys to multiple values in a single atomic operation
if none of the keys already exist.")

(def-cmd INCR (key) :integer
  "Increment the integer value of KEY.")

(def-cmd INCRBY (key increment) :integer
  "Increment the integer value of KEY by .")

(def-cmd INCRBYFLOAT (key increment) :float
  "Increment the float value of KEY by INCREMENT.")

(def-cmd DECR (key) :integer
  "Decrement the integer value of KEY.")

(def-cmd DECRBY (key decrement) :integer
  "Decrement the integer value of KEY by DECREMENT.")

(def-cmd APPEND (key value) :integer
  "Append the specified string to the string stored at key.")

(def-cmd SUBSTR (key start end) :bulk
  "Return a substring out of a larger string.
Warning: left for backwards compatibility. It is now called: GETRANGE.")

(def-cmd STRLEN (key) :integer
  "Returns the length of the string value stored at KEY.")

(def-cmd SETBIT (key offset value) :integer
  "Sets or clears the bit at OFFSET in the string value stored at KEY.")

(def-cmd GETBIT (key offset) :integer
  "Returns the bit value at OFFSET in the string value stored at KEY.")

(def-cmd BITCOUNT (key &optional start end) :integer
  "Count set bits in a string at KEY
\(with optional bounding indices START and END).")

(defmethod tell :before ((cmd (eql 'BITCOUNT)) &rest args)
  (assert (or (null (second args)) (third args))))

(def-cmd BITOP (operation destkey key &rest keys) :integer
  "Perform bitwise OPERATION between strings ar KEY and KEYS
and store the result ad DSTKEY.")

(def-cmd SETRANGE (key offset value) :integer
  "Overwrites part of the string stored at KEY, starting at the specified
OFFSET, for the entire length of VALUE. If the OFFSET is larger than the
current length of the string at KEY, the string is padded with zero-bytes
to make OFFSET fit. Non-existing keys are considered as empty strings,
so this command will make sure it holds a string large enough to be able
to set value at OFFSET. Note that the maximum OFFSET that you can set
is 229^-1 (536870911), as Redis Strings are limited to 512 megabytes.")

(def-cmd GETRANGE (key offset value) :bulk
  "Returns the substring of the string value stored at key, determined by
the offsets START and END (both are inclusive). Negative offsets can be
used in order to provide an offset starting from the end of the string.
So -1 means the last character, -2 the penultimate and so forth.")


;; Hash commands

(def-cmd HSET (key field value) :boolean
  "Set the hash FIELD to the specified VALUE. Creates the hash if needed.")

(def-cmd HSETNX (key field value) :boolean
  "Set the hash FIELD to the specified VALUE, if the KEY doesn't exist yet.")

(def-cmd HGET (key field) :bulk
  "Retrieve the value of the specified hash FIELD.")

(def-cmd HMSET (key &rest fields-and-values) :status
  "Set the hash FIELDS to their respective VALUES.")

(def-cmd HMGET (key field &rest fields) :multi
  "Get the values associated with the specified FIELDS in the hash
stored at KEY.")

(def-cmd HINCRBY (key field integer) :integer
  "Increment the integer value of the hash at KEY on FIELD with INTEGER.")

(def-cmd HINCRBYFLOAT (key field increment) :float
  "Increment the float value of the hash at KEY on FIELD with INCREMENT.")

(def-cmd HEXISTS (key field) :boolean
  "Test for existence of a specified FIELD in a hash.")

(def-cmd HDEL (key field) :boolean
  "Remove the specified FIELD from a hash.")

(def-cmd HLEN (key) :integer
  "Return the number of items in a hash.")

(def-cmd HKEYS (key) :multi
  "Return all the fields in a hash.")

(def-cmd HVALS (key) :multi
  "Return all the values in a hash.")

(def-cmd HGETALL (key) :multi
  "Return all the fields and associated values in a hash.")


;;; List commands

(def-cmd RPUSH (key value) :integer
  "Append an element to the tail of the list value at KEY.")

(def-cmd LPUSH (key value) :integer
  "Append an element to the head of the list value at KEY.")

(def-cmd RPUSHX (key value) :integer
  "Inserts value at the tail of the list stored at KEY, only if KEY
already exists and holds a list. In contrary to RPUSH, no operation
will be performed when KEY does not yet exist.")

(def-cmd LPUSHX (key value) :integer
  "Inserts value at the head of the list stored at KEY, only if KEY
already exists and holds a list. In contrary to LPUSH, no operation
will be performed when KEY does not yet exist.")

(def-cmd LLEN (key) :integer
  "Return the length of the List value at key.")

(def-cmd LRANGE (key start end) :multi
  "Return a range of elements from the List at key.")

(def-cmd LTRIM (key start end) :status
  "Trim the list at key to the specified range of elements.")

(def-cmd LINDEX (key index) :bulk
  "Return the element at index position from the List at key.")

(def-cmd LSET (key index value) :status
  "Set a new value as the element at index position of the List at key.")

(def-cmd LREM (key count value) :integer
  "Remove the first-N, last-N, or all the elements matching value from
the List at key.")

(def-cmd LPOP (key) :bulk
  "Return and remove (atomically) the first element of the List at key.")

(def-cmd RPOP (key) :bulk
  "Return and remove (atomically) the last element of the List at key.")

(def-cmd BLPOP (&rest keys-and-timeout) :multi
  "Blocking LPOP.")

(def-cmd BRPOP (&rest keys-and-timeout) :multi
  "Blocking RPOP.")

(def-cmd RPOPLPUSH (source destination) :bulk
  "Atomically returns and removes the last element (tail) of the list
stored at SOURCE, and pushes the element at the first element (head) of the list
stored at DESTINATION.
For example: consider SOURCE holding the list a,b,c, and DESTINATION holding
the list x,y,z. Executing RPOPLPUSH results in SOURCE holding a,b and
DESTINATION holding c,x,y,z.
If SOURCE does not exist, the value nil is returned and no operation is
performed. If SOURCE and DESTINATION are the same, the operation is equivalent
to removing the last element from the list and pushing it as first element
of the list, so it can be considered as a list rotation command.")

(def-cmd BRPOPLPUSH (source destination timeout) :anything  ; bulk or null multi-bulk
  "BRPOPLPUSH is the blocking variant of RPOPLPUSH. When source contains
elements, this command behaves exactly like RPOPLPUSH. When source is empty,
Redis will block the connection until another client pushes to it or until
TIMEOUT is reached. A TIMEOUT of zero can be used to block indefinitely.
See RPOPLPUSH for more information.")

(def-cmd LINSERT (key before/after pivot value) :integer
  "Inserts VALUE in the list stored at KEY either BEFORE or AFTER
the reference value PIVOT. When KEY does not exist, it is considered an empty
list and no operation is performed. An error is returned when KEY exists,
but does not hold a list value PIVOT.

Note: before/after can only have 2 values: :before or :after.")

(defmethod tell :before ((cmd (eql 'LINSERT)) &rest args)
  (assert (member (second args) '(:before :after))))


;;; Set commands

(def-cmd SADD (key member) :boolean
  "Add the specified member to the Set value at key.")

(def-cmd SREM (key member) :boolean
  "Remove the specified member from the Set value at key.")

(def-cmd SPOP (key) :bulk
  "Remove and return (pop) a random element from the Set value at key.")

(def-cmd SMOVE (srckey dstkey member) :boolean
  "Move the specified member from one Set to another atomically.")

(def-cmd SCARD (key) :integer
  "Return the number of elements (the cardinality) of the Set at key.")

(def-cmd SISMEMBER (key member) :boolean
  "Test if the specified value is a member of the Set at key.")

(def-cmd SINTER (&rest keys) :multi
  "Return the intersection between the Sets stored at key1, key2, ...,
keyN.")

(def-cmd SINTERSTORE (dstkey &rest keys) :integer
  "Compute the intersection between the Sets stored at key1, key2,
..., keyN, and store the resulting Set at dstkey.")

(def-cmd SUNION (&rest keys) :multi
  "Return the union between the Sets stored at key1, key2, ..., keyN.")

(def-cmd SUNIONSTORE (dstkey &rest keys) :integer
  "Compute the union between the Sets stored at key1, key2, ..., keyN,
and store the resulting Set at dstkey.")

(def-cmd SDIFF (&rest keys) :multi
  "Return the difference between the Set stored at key1 and all the
Sets key2, ..., keyN.")

(def-cmd SDIFFSTORE (dstkey &rest keys) :integer
  "Compute the difference between the Set key1 and all the Sets key2,
..., keyN, and store the resulting Set at dstkey.")

(def-cmd SMEMBERS (key) :multi
  "Return all the members of the Set value at key.")

(def-cmd SRANDMEMBER (key &optional count) :anything
  "Get one or COUNT random members from a set at KEY.
When called with the additional count argument,
return an array of count distinct elements if count is positive.
If called with a negative count the behavior changes and the command
is allowed to return the same element multiple times.
In this case the numer of returned elements is the absolute
value of the specified count.")


;;; Sorted set (zset) commands

(def-cmd ZADD (key score member) :boolean
  "Add the specified MEMBER to the Set value at KEY or update the
SCORE if it already exist.  If nil is returned, the element already
existed in the set.  Just the score was updated.")

(def-cmd ZREM (key member) :boolean
  "Remove the specified MEMBER from the Set value at KEY.")

(def-cmd ZINCRBY (key increment member) :integer
  "If the MEMBER already exists increment its score by INCREMENT,
otherwise add the member setting INCREMENT as score.")

(def-cmd ZRANK (key member) :integer
  "Return the rank (or index) or MEMBER in the sorted set at KEY,
with scores being ordered from low to high.")

(def-cmd ZREVRANK (key member) :integer
  "Return the rank (or index) or MEMBER in the sorted set at KEY,
with scores being ordered from high to low.")

(def-cmd ZRANGE (key start end &optional withscores) :multi
  "Return a range of elements from the sorted set at KEY.")

(def-cmd ZREVRANGE (key start end &optional withscores) :multi
  "Return a range of elements from the sorted set at KEY, exactly like
ZRANGE, but the sorted set is ordered in traversed in reverse order,
from the greatest to the smallest score.")

(macrolet ((proper-withscores ()
             `(when (and (= 4 (length args))
                         (last1 args))
                (setf (car (last args)) :withscores))))
  (defmethod tell :before ((cmd (eql 'ZRANGE)) &rest args)
    (proper-withscores))
  (defmethod tell :before ((cmd (eql 'ZREVRANGE)) &rest args)
    (proper-withscores)))

(def-cmd ZRANGEBYSCORE (key min max &rest args &key withscores limit) :multi
  "Returns all the elements in the sorted set at KEY with a score between
MIN and MAX (including elements with score equal to MIN or MAX).
The elements are considered to be ordered from low to high scores.
The elements having the same score are returned in lexicographical order (this
follows from a property of the sorted set implementation in Redis and does not
involve further computation).
The optional LIMIT argument can be used to only get a range of the matching
elements (similar to SELECT LIMIT offset, count in SQL).
The optional WITHSCORES argument makes the command return both the element and
its score, instead of the element alone.")

(def-cmd ZREVRANGEBYSCORE (key max min &rest args &key withscores limit) :multi
  "Returns all the elements in the sorted set at KEY with a score between
MAX and MIN (including elements with score equal to MAX or MIN).
In contrary to the default ordering of sorted sets, for this command the
elements are considered to be ordered from high to low scores.
The elements having the same score are returned in reverse lexicographical order.
Apart from the reversed ordering, ZREVRANGEBYSCORE is similar to ZRANGEBYSCORE.")

(flet ((send-request (cmd key start end &key withscores limit)
         (apply #'tell (princ-to-string cmd)
                (cl:append (list key start end)
                           (when withscores '("WITHSCORES"))
                           (when limit
                             (assert (and (consp limit)
                                          (atom (cdr limit))))
                             (list "LIMIT" (car limit) (cdr limit)))))))
  (defmethod tell ((cmd (eql 'ZRANGEBYSCORE)) &rest args)
    (apply #'send-request cmd args))
  (defmethod tell ((cmd (eql 'ZREVRANGEBYSCORE)) &rest args)
    (apply #'send-request cmd args)))

(def-cmd ZCARD (key) :integer
  "Return the cardinality (number of elements) of the sorted set at KEY.")

(def-cmd ZCOUNT (key min max) :integer
  "Returns the number of elements in the sorted set at KEY with a score between
MIN and MAX.")

(def-cmd ZSCORE (key element) :float
  "Return the score associated with the specified ELEMENT of the
sorted set at KEY.")

(def-cmd ZREMRANGEBYRANK (key min max) :integer
  "Remove all the elements with rank >= MIN and rank <= MAX from the
sorted set.")

(def-cmd ZREMRANGEBYSCORE (key min max) :integer
  "Remove all the elements with score >= MIN and score <= MAX from the
sorted set.")

(def-cmd ZUNIONSTORE (dstkey n keys &rest args &key weights aggregate) :integer
  "Perform a union in DSTKEY over a number (N) of sorted sets at KEYS
with optional WEIGHTS and AGGREGATE.")

(def-cmd ZINTERSTORE (dstkey n keys &rest args &key weights aggregate) :integer
  "Perform an intersection in DSTKEY over a number (N) of sorted sets at KEYS
with optional WEIGHTS and AGGREGATE.")

(flet ((send-request (cmd dstkey n keys &key weights aggregate)
         (assert (integerp n))
         (assert (= n (length keys)))
         (when weights
           (assert (= (length keys) (length weights)))
           (assert (every #'numberp weights)))
         (when aggregate
           (assert (member aggregate '(:sum :min :max))))
         (apply #'tell (princ-to-string cmd)
                (cl:append (list dstkey n)
                           keys
                           (when weights (cons "WEIGHTS" weights))
                           (when aggregate (list "AGGREGATE" aggregate))))))
  (defmethod tell ((cmd (eql 'ZUNIONSTORE)) &rest args)
    (apply #'send-request cmd args))
  (defmethod tell ((cmd (eql 'ZINTERSTORE)) &rest args)
    (apply #'send-request cmd args)))


;;; Transaction commands

(def-cmd MULTI () :status
  "Redis atomic transactions' start.")

(def-cmd EXEC () :queued
  "Redis atomic transactions' commit.")

(def-cmd DISCARD () :status
  "Redis atomic transactions' rollback.")

(def-cmd WATCH (key &rest keys) :status
  "Marks the given keys to be watched for conditional execution of a transaction.")

(def-cmd UNWATCH () :status
  "Flushes all the previously watched keys for a transaction.
If you call EXEC or DISCARD, there's no need to manually call UNWATCH.")


;;; Publish/Subscribe

(def-cmd SUBSCRIBE (&rest channels) :pubsub
  "Redis Public/Subscribe messaging paradigm implementation.")

(def-cmd UNSUBSCRIBE (&rest channels) :pubsub
  "Redis Public/Subscribe messaging paradigm implementation.")

(def-cmd PSUBSCRIBE (&rest patterns) :pubsub
  "Redis Public/Subscribe messaging paradigm implementation.")

(def-cmd PUNSUBSCRIBE (&rest patterns) :pubsub
  "Redis Public/Subscribe messaging paradigm implementation.")

(def-cmd PUBLISH (channel message) :integer
  "Redis Public/Subscribe messaging paradigm implementation.")


;;; Server control commands

(def-cmd SAVE () :status
  "Synchronously save the DB on disk.")

(def-cmd BGSAVE () :inline
  "Asynchronously save the DB on disk.")

(def-cmd LASTSAVE () :integer
  "Return the UNIX time stamp of the last successfully saving of the
dataset on disk.")

(def-cmd SHUTDOWN () :end
  "Synchronously save the DB on disk, then shutdown the server.")

(def-cmd BGREWRITEAOF () :status
  "Rewrite the append only file in background when it gets too big.")

(def-cmd INFO () :bulk
  "Provide information and statistics about the server.")

(def-cmd SLAVEOF (hostname port) :status
  "Change the replication settings.")

(def-cmd CONFIG-GET (pattern) :multi
  "Configure a Redis server at runtime: get glob PATTERN value.")

(def-cmd CONFIG-SET (parameter value) :status
  "Configure a Redis server at runtime: set PARAMETER VALUE.")

(def-cmd CONFIG-RESETSTAT () :status
  "Resets the statistics reported by Redis using the INFO command.
These are the counters that are reset:
Keyspace hits
Keyspace misses
Number of commands processed
Number of connections received
Number of expired keys")

(def-cmd FLUSHDB () :status
  "Remove all the keys of the currently selected DB.")

(def-cmd FLUSHALL () :status
  "Remove all the keys from all the databases.")

(def-cmd DBSIZE () :integer
  "Return the number of keys in the current db.")

(def-cmd SYNC () :multi
  "Synchronize with slave.")

(def-cmd SLOWLOG (subcommand &optional argument) :anything
  "Manages the Redis slow queries log.")


;;; Scripting commands

(def-cmd EVAL (script numkeys &rest key-values) :anything
  "Execute a Lua script server side.")

(def-cmd EVALSHA (sha1 numkeys &rest key-values) :anything
  "Execute a stored Lua script server side.")

(def-cmd SCRIPT-LOAD (script) :bulk
  "Load the specified Lua script into the script cache.")

(def-cmd SCRIPT-EXISTS (script &rest scripts) :multi
  "Check existence of scripts in the script cache.")

(def-cmd SCRIPT-KILL () :status
  "Kill the script currently in execution.")

(def-cmd SCRIPT-FLUSH () :status
  "Remove all the scripts from the script cache.")


;;; not supported commands: MONITOR, DEBUG OBJECT, DEBUG SEGFAULT - use redis-cli for that

;;; end
