;;; CL-REDIS commands
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions


(in-package :redis)

;; Connection handling

(def-cmd PING ()
  "Ping server."
  :inline :status)

(def-cmd QUIT ()
  "Close the connection."
  :inline :end)

(def-cmd AUTH (pass)
  "Simple password authentication if enabled."
  :inline :status)


;; Commands operating on all the kind of values
    
(def-cmd EXISTS (key)
  "Test if a key exists."
  :inline :boolean)

(def-cmd DEL (key &rest keys)
  "Delete a key."
  :inline :integer)

(def-cmd TYPE (key)
  "Return the type of the value stored at key."
  :inline :inline)

(def-cmd KEYS (pattern)
  "Return all the keys matching a given pattern."
  :inline :multi)

(def-cmd RANDOMKEY ()
  "Return a random key from the key space."
  :inline :bulk)

(def-cmd RENAME (oldname newname)
  "Rename the old key in the new one, destroing the newname key if it
already exists."
  :inline :status)

(def-cmd RENAMENX (oldname newname)
  "Rename the old key in the new one, if the newname key does not
already exist."
 :inline :boolean)
  
(def-cmd DBSIZE ()
  "Return the number of keys in the current db."
  :inline :integer)

(def-cmd EXPIRE (key secs)
  "Set a time to live in seconds on a key." 
  :inline :boolean)

(def-cmd TTL (key)
  "Get the time to live in seconds of a key."
  :inline :integer)

(def-cmd SELECT (index)
  "Select the DB having the specified index."
  :inline :status)

(def-cmd MOVE (key dbindex)
  "Move the key from the currently selected DB to the DB having as
index dbindex."
  :inline :status)

(def-cmd FLUSHDB ()
  "Remove all the keys of the currently selected DB."
  :inline :status)

(def-cmd FLUSHALL ()
  "Remove all the keys from all the databases."
  :inline :status)


;; Commands operating on string values

(def-cmd SET (key value)
  "Set a key to a string value."
  :bulk :status)

(def-cmd GET (key)
  "Return the string value of the key."
  :inline :bulk)

(def-cmd GETSET (key value)
  "Set a key to a string returning the old value of the key."
  :bulk :bulk)

(def-cmd MGET (&rest keys)
  "Multi-get, return the strings values of the keys."
  :inline :multi)

(def-cmd SETNX (key value)
  "Set a key to a string value if the key does not exist."
  :bulk :boolean)

(def-cmd SETEX (key time value)
  "Set+Expire combo command"
  :bulk :status)

(def-cmd MSET (&rest key-value-plist)
  "Set multiple keys to multiple values in a single atomic operation."
  :multi :status)

(def-cmd MSETNX (&rest key-value-plist)
  "Set multiple keys to multiple values in a single atomic operation
if none of the keys already exist."
  :multi :boolean)

(def-cmd INCR (key)
  "Increment the integer value of key."
  :inline :integer)

(def-cmd INCRBY (key integer)
  "Increment the integer value of key by integer."
  :inline :integer)

(def-cmd DECR (key)
  "Decrement the integer value of key."
  :inline :integer)

(def-cmd DECRBY (key integer)
  "Decrement the integer value of key by integer."
  :inline :integer)

(def-cmd APPEND (key value)
  "Append the specified string to the string stored at key."
  :bulk :integer)

(def-cmd SUBSTR (key start end)
  "Return a substring out of a larger string."
  :inline :bulk)


;; Commands operating on lists

(def-cmd RPUSH (key value)
  "Append an element to the tail of the List value at key."
  :bulk :integer)
  
(def-cmd LPUSH (key value)
  "Append an element to the head of the List value at key."
  :bulk :integer)

(def-cmd LLEN (key)
  "Return the length of the List value at key."
  :inline :integer)

(def-cmd LRANGE (key start end)
  "Return a range of elements from the List at key."
  :inline :multi)

(def-cmd LTRIM (key start end)
  "Trim the list at key to the specified range of elements." 
  :inline :status)

(def-cmd LINDEX (key index)
  "Return the element at index position from the List at key."
  :inline :bulk)

(def-cmd LSET (key index value)
  "Set a new value as the element at index position of the List at key."
  :bulk :status)

(def-cmd LREM (key count value)
  "Remove the first-N, last-N, or all the elements matching value from
the List at key."
  :bulk :integer)

(def-cmd LPOP (key)
  "Return and remove (atomically) the first element of the List at key."
  :inline :bulk)

(def-cmd RPOP (key)
  "Return and remove (atomically) the last element of the List at key."
  :inline :bulk)

(def-cmd BLPOP (&rest keys-and-timeout)
  "Blocking LPOP."
  :inline :multi)

(def-cmd BRPOP (&rest keys-and-timeout)
  "Blocking RPOP."
  :inline :multi)

(def-cmd RPOPLPUSH (srckey dstkey)
  "Return and remove (atomically) the last element of the source List stored
at SRCKEY and push the same element to the destination List stored at DSTKEY."
  :inline :bulk)


;; Commands operating on sets

(def-cmd SADD (key member)
  "Add the specified member to the Set value at key."
  :bulk :boolean)

(def-cmd SREM (key member)
  "Remove the specified member from the Set value at key."
  :bulk :boolean)

(def-cmd SPOP (key)
  "Remove and return (pop) a random element from the Set value at key."
  :inline :bulk)

(def-cmd SMOVE (srckey dstkey member)
  "Move the specified member from one Set to another atomically."
  :bulk :boolean)
  
(def-cmd SCARD (key)
  "Return the number of elements (the cardinality) of the Set at key."
  :inline :integer)

(def-cmd SISMEMBER (key member)
  "Test if the specified value is a member of the Set at key."
  :bulk :boolean)

(def-cmd SINTER (&rest keys)
  "Return the intersection between the Sets stored at key1, key2, ...,
keyN."
  :inline :multi)

(def-cmd SINTERSTORE (dstkey &rest keys)
  "Compute the intersection between the Sets stored at key1, key2,
..., keyN, and store the resulting Set at dstkey."
  :inline :integer)

(def-cmd SUNION (&rest keys)
  "Return the union between the Sets stored at key1, key2, ..., keyN."
  :inline :multi)

(def-cmd SUNIONSTORE (dstkey &rest keys)
  "Compute the union between the Sets stored at key1, key2, ..., keyN,
and store the resulting Set at dstkey."
  :inline :integer)

(def-cmd SDIFF (&rest keys)
  "Return the difference between the Set stored at key1 and all the
Sets key2, ..., keyN."
  :inline :multi)

(def-cmd SDIFFSTORE (dstkey &rest keys)
  "Compute the difference between the Set key1 and all the Sets key2,
..., keyN, and store the resulting Set at dstkey."
  :inline :integer)

(def-cmd SMEMBERS (key)
  "Return all the members of the Set value at key."
  :inline :multi)

(def-cmd SRANDMEMBER (key)
  "Return a random member of the Set value at KEY."
  :inline :bulk)


;; Commands operating on sorted sets (zsets)

(def-cmd ZADD (key score member)
  "Add the specified MEMBER to the Set value at KEY or update the
SCORE if it already exist.  If nil is returned, the element already
existed in the set.  Just the score was updated."
  :bulk :boolean)

(def-cmd ZREM (key member)
  "Remove the specified MEMBER from the Set value at KEY."
  :bulk :boolean)

(def-cmd ZINCRBY (key increment member)
  "If the MEMBER already exists increment its score by INCREMENT,
otherwise add the member setting INCREMENT as score."
  :inline :integer)

(def-cmd ZRANK (key member)
  "Return the rank (or index) or MEMBER in the sorted set at KEY,
with scores being ordered from low to high."
  :bulk :integer)

(def-cmd ZREVRANK (key member)
  "Return the rank (or index) or MEMBER in the sorted set at KEY,
with scores being ordered from high to low."
  :bulk :integer)

(def-cmd ZRANGE (key start end &rest args &key withscores)
  "Return a range of elements from the sorted set at KEY."
  :inline :multi)

(def-cmd ZREVRANGE (key start end &rest args &key withscores)
  "Return a range of elements from the sorted set at KEY, exactly like
ZRANGE, but the sorted set is ordered in traversed in reverse order,
from the greatest to the smallest score."
  :inline :multi)

(def-cmd ZRANGEBYSCORE (key min max)
  "Return all the elements with score >= MIN and score <= MAX (a range
query) from the sorted set."
  :inline :multi)

(def-cmd ZCARD (key)
  "Return the cardinality (number of elements) of the sorted set at KEY."
  :inline :integer)

(def-cmd ZSCORE (key element)
  "Return the score associated with the specified ELEMENT of the
sorted set at KEY."
  :bulk :float)

(def-cmd ZREMRANGEBYRANK (key min max)
  "Remove all the elements with rank >= MIN and rank <= MAX from the
sorted set."
  :inline :integer)

(def-cmd ZREMRANGEBYSCORE (key min max)
  "Remove all the elements with score >= MIN and score <= MAX from the
sorted set."
  :inline :integer)

(def-cmd ZUNIONSTORE (dstkey n keys
                             &rest args &key weights aggregate)
  "Perform a union over a number of sorted sets with optional
weight and aggregate."
  :inline :integer)

(def-cmd ZINTERSTORE (dstkey n keys
                             &rest args &key weights aggregate)
  "Perform an intersection over a number of sorted sets with optional
weight and aggregate."
  :inline :integer)


;; Commands operating on hashes

(def-cmd HSET (key field value)
  "Set the hash FIELD to the specified VALUE. Creates the hash if needed."
  :bulk :integer)

(def-cmd HGET (key field)
  "Retrieve the value of the specified hash FIELD."
  :generic :bulk)

(def-cmd HMSET (key &rest fields-and-values)
  "Set the hash FIELDS to their respective VALUES."
  :multi :status)

(def-cmd HMGET (key &rest fields)
  "Get the hash values associated with the specified fields."
  :generic :multi)

(def-cmd HINCRBY (key field integer)
  "Increment the integer value of the hash at KEY on FIELD with INTEGER."
  :inline :integer)

(def-cmd HEXISTS (key field)
  "Test for existence of a specified FIELD in a hash."
  :generic :boolean)

(def-cmd HDEL (key field)
  "Remove the specified FIELD from a hash."
  :generic :boolean)

(def-cmd HLEN (key)
  "Return the number of items in a hash."
  :inline :integer)

(def-cmd HKEYS (key)
  "Return all the fields in a hash."
  :inline :multi)

(def-cmd HVALS (key)
  "Return all the values in a hash."
  :inline :multi)

(def-cmd HGETALL (key)
  "Return all the fields and associated values in a hash."
  :inline :multi)


;; Sorting

(def-cmd SORT (key &rest args
                   &key  by     ; A pattern.
                         start  
                         end
                         get    ; A pattern or a list of patterns.
                         desc   ; Should sort be descending?
                         alpha  ; Should sort be lexicographical?
                         )
  "Sort a Set or a List accordingly to the specified parameters."
  :inline :multi)


;; Transactions

(def-cmd MULTI ()
  "Redis atomic transactions' start."
  :inline :status)

(def-cmd EXEC ()
  "Redis atomic transactions' commit."
  :inline :queued)

(def-cmd DISCARD ()
  "Redis atomic transactions' rollback."
  :inline :status)


;; Publish/Subscribe

(def-cmd SUBSCRIBE (&rest channels)
  "Redis Public/Subscribe messaging paradigm implementation."
  :inline :pubsub)

(def-cmd UNSUBSCRIBE (&rest channels)
  "Redis Public/Subscribe messaging paradigm implementation."
  :inline :pubsub)

(def-cmd PSUBSCRIBE (&rest patterns)
  "Redis Public/Subscribe messaging paradigm implementation."
  :inline :pubsub)

(def-cmd PUNSUBSCRIBE (&rest patterns)
  "Redis Public/Subscribe messaging paradigm implementation."
  :inline :pubsub)

(def-cmd PUBLISH (channel message)
  "Redis Public/Subscribe messaging paradigm implementation."
  :bulk :integer)


;; Persistence control commands

(def-cmd SAVE ()
  "Synchronously save the DB on disk."
  :inline :status)

(def-cmd BGSAVE ()
  "Asynchronously save the DB on disk."
  :inline :inline)

(def-cmd LASTSAVE ()
  "Return the UNIX time stamp of the last successfully saving of the
dataset on disk."
  :inline :integer)

(def-cmd SHUTDOWN ()
  "Synchronously save the DB on disk, then shutdown the server."
  :inline :end)

(def-cmd BGREWRITEAOF ()
  "Rewrite the append only file in background when it gets too big."
  :inline :status)


;; Remote server control commands

(def-cmd INFO ()
  "Provide information and statistics about the server."
  :inline :bulk)

(def-cmd MONITOR ()
  "Dump all the received requests in real time."
  :inline :status)

(def-cmd SLAVEOF (master)
  "Change the replication settings."
  :inline :status)

(def-cmd CONFIG-GET (pattern)
  "Configure a Redis server at runtime: get glob PATTERN value."
  :inline :bulk)

(def-cmd CONFIG-SET (parameter value)
  "Configure a Redis server at runtime: set PARAMETER VALUE."
  :inline :status)

;;; end