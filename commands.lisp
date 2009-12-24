;;; CL-REDIS commands
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions


(in-package :redis)

(def-cmd PING ()
  "Ping server."
  :inline :pong)

(def-cmd QUIT ()
  "Close the connection."
  :inline :end)
    
(def-cmd AUTH (pass)
  "Simple password authentication if enabled."
  :inline :ok)

(def-cmd SET (key value)
  "Set a key to a string value."
  :bulk :ok)

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

(def-cmd MSET (&rest key-value-plist)
  "Set multiple keys to multiple values in a single atomic operation."
  :multi :ok)

(def-cmd MSETNX (&rest key-value-plist)
  "Set multiple keys to multiple values in a single atomic operation if none of the keys already exist."
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
  :inline :list)

(def-cmd RANDOMKEY ()
  "Return a random key from the key space."
  :inline :inline)

(def-cmd RENAME (oldname newname)
  "Rename the old key in the new one, destroing the newname key ~
if it already exists."
  :inline :ok)

(def-cmd RENAMENX (oldname newname)
  "Rename the old key in the new one, if the newname key ~
does not already exist."
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

(def-cmd RPUSH (key value)
  "Append an element to the tail of the List value at key."
  :bulk :ok)
  
(def-cmd LPUSH (key value)
  "Append an element to the head of the List value at key."
  :bulk :ok)

(def-cmd LLEN (key)
  "Return the length of the List value at key."
  :inline :integer)

(def-cmd LRANGE (key start end)
  "Return a range of elements from the List at key."
  :inline :multi)

(def-cmd LTRIM (key start end)
  "Trim the list at key to the specified range of elements." 
  :inline :ok)

(def-cmd LINDEX (key index)
  "Return the element at index position from the List at key."
  :inline :bulk)

(def-cmd LSET (key index value)
  "Set a new value as the element at index position of the List ~
at key."
  :bulk :ok)

(def-cmd LREM (key count value)
  "Remove the first-N, last-N, or all the elements matching value ~
from the List at key."
  :bulk :integer)

(def-cmd LPOP (key)
  "Return and remove (atomically) the first element of the List~
 at key."
  :inline :bulk)

(def-cmd RPOP (key)
  "Return and remove (atomically) the last element of the List ~
at key."
  :inline :bulk)

(def-cmd SADD (key member)
  "Add the specified member to the Set value at key."
  :bulk :boolean)

(def-cmd SREM (key member)
  "Remove the specified member from the Set value at key."
  :bulk :boolean)

(def-cmd SPOP (key)
  "Remove and return (pop) a random element from the Set value ~
at key."
  :inline :bulk)

(def-cmd SMOVE (srckey dstkey member)
  "Move the specified member from one Set to another atomically."
  :bulk :boolean)
  
(def-cmd SCARD (key)
  "Return the number of elements (the cardinality) of the Set ~
at key."
  :inline :integer)

(def-cmd SISMEMBER (key member)
  "Test if the specified value is a member of the Set at key."
  :bulk :boolean)

(def-cmd SINTER (&rest keys)
  "Return the intersection between the Sets stored ~
at key1, key2, ..., keyN."
  :inline :multi)

(def-cmd SINTERSTORE (dstkey &rest keys)
  "Compute the intersection between the Sets stored ~
at key1, key2, ..., keyN, and store the resulting Set at dstkey."
  :inline :integer)

(def-cmd SUNION (&rest keys)
  "Return the union between the Sets stored ~
at key1, key2, ..., keyN."
  :inline :multi)

(def-cmd SUNIONSTORE (dstkey &rest keys)
  "Compute the union between the Sets stored ~
at key1, key2, ..., keyN, and store the resulting Set at dstkey."
  :inline :integer)

(def-cmd SDIFF (&rest keys)
  "Return the difference between the Set stored ~
at key1 and all the Sets key2, ..., keyN."
  :inline :multi)

(def-cmd SDIFFSTORE (dstkey &rest keys)
  "Compute the difference between the Set key1 and ~
all the Sets key2, ..., keyN, and store the resulting Set at dstkey."
  :inline :integer)

(def-cmd SMEMBERS (key)
  "Return all the members of the Set value at key."
  :inline :multi)

(def-cmd ZADD (key score member)
  "Add the specified member to the Set value at key or ~
update the score if it already exist.
If nil is returned, the element already existed in the set. Just the score ~
was updated."
  :bulk :boolean)

(def-cmd ZREM (key member)
  "Remove the specified member from the Set value at key."
  :bulk :boolean)

(def-cmd ZRANGE (key start end)
  "Return a range of elements from the sorted set at key."
  :inline :multi)

(def-cmd ZREVRANGE (key start end)
  "Return a range of elements from the sorted set at key, ~
exactly like ZRANGE, but the sorted set is ordered in traversed in reverse ~
order, from the greatest to the smallest score."
  :inline :multi)

(def-cmd ZRANGEBYSCORE (key min max)
  "Return all the elements with score >= min and ~
score <= max (a range query) from the sorted set."
  :inline :multi)

(def-cmd ZCARD (key)
  "Return the cardinality (number of elements) of the sorted set ~
at key."
  :inline :integer)

(def-cmd ZSCORE (key element)
  "Return the score associated with the specified element of the ~
sorted set at key."
  :bulk :bulk)

(def-cmd ZREMRANGEBYSCORE (key min max)
  "Remove all the elements with score >= min and ~
score <= max from the sorted set."
  :inline :integer)

(def-cmd SELECT (index)
  "Select the DB having the specified index."
  :inline :ok)

(def-cmd MOVE (key dbindex)
  "Move the key from the currently selected DB to ~
the DB having as index dbindex."
  :inline :ok)

(def-cmd FLUSHDB ()
  "Remove all the keys of the currently selected DB."
  :inline :ok)

(def-cmd FLUSHALL ()
  "Remove all the keys from all the databases."
  :inline :ok)

(def-cmd SORT (key &rest args
                   &key by     ; pattern
                   start  
                   count  
                   get    ; pattern or a list of patterns
                   desc   ; should sort be descending? default is NIL
                   alpha  ; should sort be lexicographical? default is NIL
                   )
  "Sort a Set or a List accordingly to the specified parameters."
  :inline :multi)

(def-cmd SAVE ()
  "Synchronously save the DB on disk."
  :inline :ok)

(def-cmd BGSAVE ()
  "Asynchronously save the DB on disk."
  :inline :ok)

(def-cmd LASTSAVE ()
  "Return the UNIX time stamp of the last successfully ~
saving of the dataset on disk."
  :inline :integer)

(def-cmd SHUTDOWN ()
  "Synchronously save the DB on disk, then shutdown the server."
  :inline :end)

(def-cmd INFO ()
  "Provide information and statistics about the server."
  :inline :bulk)

#+nil
(def-cmd MONITOR ()
  "Dump all the received requests in real time."
  :inline :ok)

#+nil
(def-cmd SLAVEOF (master)
  "Change the replication settings."
  :inline :ok)

;;; end