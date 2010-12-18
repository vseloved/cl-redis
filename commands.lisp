;;; CL-REDIS commands
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions


(in-package :redis)

;; Connection handling

(def-cmd PING () :status
  "Ping server.")

(def-cmd QUIT () :end
  "Close the connection.")

(def-cmd AUTH (pass) :status
  "Simple password authentication if enabled.")


;; Commands operating on all the kind of values
    
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
  
(def-cmd DBSIZE () :integer
  "Return the number of keys in the current db.")

(def-cmd EXPIRE (key secs) :boolean 
  "Set a time to live in seconds on a key.")

(def-cmd TTL (key) :integer
  "Get the time to live in seconds of a key.")

(def-cmd SELECT (index) :status
  "Select the DB having the specified index.")

(def-cmd MOVE (key dbindex) :integer
  "Move the key from the currently selected DB to the DB having as
index dbindex.")

(def-cmd FLUSHDB () :status
  "Remove all the keys of the currently selected DB.")

(def-cmd FLUSHALL () :status
  "Remove all the keys from all the databases.")


;; Commands operating on string values

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
  "Set+Expire combo command")

(def-cmd MSET (&rest key-value-plist) :status
  "Set multiple keys to multiple values in a single atomic operation.")

(def-cmd MSETNX (&rest key-value-plist) :boolean
  "Set multiple keys to multiple values in a single atomic operation
if none of the keys already exist.")

(def-cmd INCR (key) :integer
  "Increment the integer value of key.")

(def-cmd INCRBY (key integer) :integer
  "Increment the integer value of key by integer.")

(def-cmd DECR (key) :integer
  "Decrement the integer value of key.")

(def-cmd DECRBY (key integer) :integer
  "Decrement the integer value of key by integer.")

(def-cmd APPEND (key value) :integer
  "Append the specified string to the string stored at key.")

(def-cmd SUBSTR (key start end) :bulk
  "Return a substring out of a larger string.")


;; Commands operating on lists

(def-cmd RPUSH (key value) :integer
  "Append an element to the tail of the List value at key.")
  
(def-cmd LPUSH (key value) :integer
  "Append an element to the head of the List value at key.")

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

(def-cmd RPOPLPUSH (srckey dstkey) :bulk
  "Return and remove (atomically) the last element of the source List stored
at SRCKEY and push the same element to the destination List stored at DSTKEY.")


;; Commands operating on sets

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

(def-cmd SRANDMEMBER (key) :bulk
  "Return a random member of the Set value at KEY.")


;; Commands operating on sorted sets (zsets)

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

(def-cmd ZRANGE (key start end &rest args &key withscores) :multi
  "Return a range of elements from the sorted set at KEY.")

(def-cmd ZREVRANGE (key start end &rest args &key withscores) :multi
  "Return a range of elements from the sorted set at KEY, exactly like
ZRANGE, but the sorted set is ordered in traversed in reverse order,
from the greatest to the smallest score.")

(def-cmd ZRANGEBYSCORE (key min max) :multi
  "Return all the elements with score >= MIN and score <= MAX (a range
query) from the sorted set.")

(def-cmd ZCARD (key) :integer
  "Return the cardinality (number of elements) of the sorted set at KEY.")

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
  "Perform a union over a number of sorted sets with optional
weight and aggregate.")

(def-cmd ZINTERSTORE (dstkey n keys &rest args &key weights aggregate) :integer
  "Perform an intersection over a number of sorted sets with optional
weight and aggregate.")


;; Commands operating on hashes

(def-cmd HSET (key field value) :integer
  "Set the hash FIELD to the specified VALUE. Creates the hash if needed.")

(def-cmd HGET (key field) :bulk
  "Retrieve the value of the specified hash FIELD.")

(def-cmd HMSET (key &rest fields-and-values) :status
  "Set the hash FIELDS to their respective VALUES.")

(def-cmd HMGET (key &rest fields) :multi
  "Get the hash values associated with the specified fields.")

(def-cmd HINCRBY (key field integer) :integer
  "Increment the integer value of the hash at KEY on FIELD with INTEGER.")

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


;; Sorting

(def-cmd SORT (key &rest args
                   &key  by     ; A pattern.
                         start  
                         end
                         get    ; A pattern or a list of patterns.
                         desc   ; Should sort be descending?
                         alpha  ; Should sort be lexicographical?
                         ) :multi
  "Sort a Set or a List accordingly to the specified parameters.")


;; Transactions

(def-cmd MULTI () :status
  "Redis atomic transactions' start.")

(def-cmd EXEC () :queued
  "Redis atomic transactions' commit.")

(def-cmd DISCARD () :status
  "Redis atomic transactions' rollback.")


;; Publish/Subscribe

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


;; Persistence control commands

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


;; Remote server control commands

(def-cmd INFO () :bulk
  "Provide information and statistics about the server.")

(def-cmd MONITOR () :status
  "Dump all the received requests in real time.")

(def-cmd SLAVEOF (master) :status
  "Change the replication settings.")

(def-cmd CONFIG-GET (pattern) :bulk
  "Configure a Redis server at runtime: get glob PATTERN value.")

(def-cmd CONFIG-SET (parameter value) :status
  "Configure a Redis server at runtime: set PARAMETER VALUE.")

;;; end