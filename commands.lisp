;;; CL-REDIS commands
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions


(in-package :redis)

(def-cmd PING "Ping server"
  :inline () :pong)

(def-cmd QUIT "Close the connection"
  :inline () :end)
    
(def-cmd AUTH "Simple password authentication if enabled"
  :inline (pass) :ok)

(def-cmd SET "Set a key to a string value"
  :bulk (key value) :ok)

(def-cmd GET "Return the string value of the key"
  :inline (key) :bulk)

(def-cmd GETSET "Set a key to a string returning the old value of the key"
  :bulk (key value) :bulk)

(def-cmd MGET "Multi-get, return the strings values of the keys"
  :inline (&rest keys) :multi)

(def-cmd SETNX "Set a key to a string value if the key does not exist"
  :bulk (key value) :boolean)

(def-cmd INCR "Increment the integer value of key"
  :inline (key) :integer)

(def-cmd INCRBY "Increment the integer value of key by integer"
  :inline (key integer) :integer)

(def-cmd DECR "Decrement the integer value of key"
  :inline (key) :integer)

(def-cmd DECRBY "Decrement the integer value of key by integer"
  :inline (key integer) :integer)

(def-cmd EXISTS "Test if a key exists"
  :inline (key) :boolean)

(def-cmd DEL "Delete a key"
  :inline (key &rest keys) :integer)

(def-cmd TYPE "Return the type of the value stored at key"
  :inline (key) :inline)

(def-cmd KEYS "Return all the keys matching a given pattern"
  :inline (pattern) :list)

(def-cmd RANDOMKEY "Return a random key from the key space"
  :inline () :inline)

(def-cmd RENAME "Rename the old key in the new one, destroing the newname key ~
if it already exists"
  :inline (oldname newname) :ok)

(def-cmd RENAMENX "Rename the old key in the new one, if the newname key ~
does not already exist"
 :inline (oldname newname) :boolean)
  
(def-cmd DBSIZE "Return the number of keys in the current db"
  :inline () :integer)

(def-cmd EXPIRE "Set a time to live in seconds on a key" 
   :inline (key secs) :boolean)

(def-cmd TTL "Get the time to live in seconds of a key"
  :inline (key) :integer)


(def-cmd RPUSH "Append an element to the tail of the List value at key"
   :bulk (key value) :ok)
  
(def-cmd LPUSH "Append an element to the head of the List value at key"
  :bulk (key value) :ok)

(def-cmd LLEN "Return the length of the List value at key"
  :inline (key) :integer)

(def-cmd LRANGE "Return a range of elements from the List at key"
  :inline (key start end) :multi)

(def-cmd LTRIM "Trim the list at key to the specified range of elements" 
  :inline (key start end) :ok)

(def-cmd LINDEX "Return the element at index position from the List at key"
  :inline (key index) :bulk)

(def-cmd LSET "Set a new value as the element at index position of the List ~
at key"
  :bulk (key index value) :ok)

(def-cmd LREM "Remove the first-N, last-N, or all the elements matching value ~
from the List at key"
  :bulk (key count value) :integer)

(def-cmd LPOP "Return and remove (atomically) the first element of the List~
 at key"
  :inline (key) :bulk)

(def-cmd RPOP "Return and remove (atomically) the last element of the List ~
at key"
  :inline (key) :bulk)


(def-cmd SADD "Add the specified member to the Set value at key"
  :bulk (key member) :boolean)

(def-cmd SREM "Remove the specified member from the Set value at key"
  :bulk (key member) :boolean)

(def-cmd SPOP "Remove and return (pop) a random element from the Set value ~
at key"
  :inline (key) :bulk)

(def-cmd SMOVE "Move the specified member from one Set to another atomically"
  :bulk (srckey dstkey member) :boolean)
  
(def-cmd SCARD "Return the number of elements (the cardinality) of the Set ~
at key"
  :inline (key) :integer)

(def-cmd SISMEMBER "Test if the specified value is a member of the Set at key"
  :bulk (key member) :boolean)

(def-cmd SINTER "Return the intersection between the Sets stored ~
at key1, key2, ..., keyN"
  :inline (&rest keys) :multi)

(def-cmd SINTERSTORE "Compute the intersection between the Sets stored ~
at key1, key2, ..., keyN, and store the resulting Set at dstkey"
  :inline (dstkey &rest keys) :integer)

(def-cmd SUNION "Return the union between the Sets stored ~
at key1, key2, ..., keyN"
  :inline (&rest keys) :multi)

(def-cmd SUNIONSTORE "Compute the union between the Sets stored ~
at key1, key2, ..., keyN, and store the resulting Set at dstkey"
  :inline (dstkey &rest keys) :integer)

(def-cmd SDIFF "Return the difference between the Set stored ~
at key1 and all the Sets key2, ..., keyN"
  :inline (&rest keys) :multi)

(def-cmd SDIFFSTORE "Compute the difference between the Set key1 and ~
all the Sets key2, ..., keyN, and store the resulting Set at dstkey"
  :inline (dstkey &rest keys) :integer)

(def-cmd SMEMBERS "Return all the members of the Set value at key"
  :inline (key) :multi)


(def-cmd ZADD "Add the specified member to the Set value at key or ~
update the score if it already exist.
If nil is returned, the element already existed in the set. Just the score ~
was updated"
  :bulk (key score member) :boolean)

(def-cmd ZREM "Remove the specified member from the Set value at key"
  :bulk (key member) :boolean)

(def-cmd ZRANGE "Return a range of elements from the sorted set at key"
  :inline (key start end) :multi)

(def-cmd ZREVRANGE "Return a range of elements from the sorted set at key, ~
exactly like ZRANGE, but the sorted set is ordered in traversed in reverse ~
order, from the greatest to the smallest score"
  :inline (key start end) :multi)

(def-cmd ZRANGEBYSCORE "Return all the elements with score >= min and ~
score <= max (a range query) from the sorted set"
  :inine (key min max) :multi)

(def-cmd ZCARD "Return the cardinality (number of elements) of the sorted set ~
at key"
  :inline (key) :integer)

(def-cmd ZSCORE "Return the score associated with the specified element of the ~
sorted set at key"
  :bulk (key element) :string)


(def-cmd SELECT "Select the DB having the specified index"
  :inline (index) :ok)

(def-cmd MOVE "Move the key from the currently selected DB to ~
the DB having as index dbindex"
  :inline (key dbindex) :ok)

(def-cmd FLUSHDB "Remove all the keys of the currently selected DB"
  :inline () :ok)

(def-cmd FLUSHALL "Remove all the keys from all the databases"
  :inline () :ok)

(def-cmd SORT "Sort a Set or a List accordingly to the specified parameters"
  :inline
  (key &rest args
       &key by     ; pattern
            limit  ; '(start end)
            get    ; pattern or a list of patterns
            desc   ; should sort be descending? default is NIL
            alpha  ; should sort be lexicographical? default is NIL
            )
  :multi)

(def-cmd SAVE "Synchronously save the DB on disk"
  :inline () :ok)

(def-cmd BGSAVE "Asynchronously save the DB on disk"
  :inline () :ok)

(def-cmd LASTSAVE "Return the UNIX time stamp of the last successfully ~
saving of the dataset on disk"
  :inline () :integer)

(def-cmd SHUTDOWN "Synchronously save the DB on disk, then shutdown the server"
  :inline () :end)

(def-cmd INFO "Provide information and statistics about the server"
  :inline () :bulk)

#+nil
(def-cmd MONITOR "Dump all the received requests in real time"
  :inline () :ok)

#+nil
(def-cmd SLAVEOF "Change the replication settings"
  :inline (master) :ok)

;;; end