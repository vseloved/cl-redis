# CL-REDIS — a fast and robust Common Lisp client for Redis
  (tested with Redis version 2.6)

## Quickstart

1. Make sure a Redis server is running.
2. `(require 'cl-redis)`
3. Connect to the server to the given host and port with
   `(redis:connect :host <host> :port <port>)`
   (`host` defaults to `127.0.0.1` and `port` - to `6739`).
4. Interact with the server using Redis commands from the `red` package.

       CL-USER> (red:ping)
       "PONG"

5. Disconnect from the server with `(redis:disconnect)`.
6. Alternatively, wrap the whole interaction session in `with-connection` macro,
   which accepts the same arguments as `connect` does, opens a socket connection,
   executes the body of the macro with the current connection bound to
   this new connection, and ensures that the connection is closed afterwards.

The system provides 2 packages: `REDIS` and `RED`.  All the functionality is
available from the `REDIS` package.  Not to cause symbol clashes,
Redis commands are defined in this packae with a prefix (which default to `red-`
and is set at compilation time).  The `RED` package is a syntactic sugar —
it just provides the Redis commands without a prefix.  So it is not intended
to be imported to avoid symbol conflicts with package `COMMON-LISP`.

So, the same Redis command (for instance `GET`) can be called as
`REDIS:RED-GET` or `RED:GET`.


## Dependencies

- [usocket](http://common-lisp.net/project/usocket/)
- [flexi-streams](http://common-lisp.net/project/flexi-streams/)
- [rutils](http://github.com/vseloved/rutils)
- only for tests: [nuts](http://github.com/vseloved/nuts),
  [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads)


## Debugging, testing and error recovery

If `*echo-p*` is `T`, all server-client communications will be
echoed to the stream `*echo-stream*`, which defaults to `*standard-output*`.

Error handling is mimicked after
[Postmodern](http://common-lisp.net/project/postmodern/).
In particular, whenever an error occurs that breaks the communication stream,
a condition of type `redis-connection-error` is signalled offering
a `:reconnect` restart.  If it is selected the whole Redis command will be
resend, if the reconnection attempt succeeds.
Furthermore, `connect` checks if a connection to Redis is already established,
and offers two restarts (`:leave` and `:replace`) if this is the case.

When the server respondes with an error reply
(i.e., a reply that starts with `-`),
a condition of type `redis-error-reply` is signalled.

There's also a high-level `with-persistent-connection` macro,
that tries to automatically reopen the connection once, if it is broken.


## PubSub example

Since there's no special command to receive messages from Redis via PubSub
here's how you do it:

    (bt:make-thread (lambda ()
                      (with-connection ()
                        (red:subscribe "foo")
                        (loop :for msg := (expect :anything) :do
                          (print msg))))
                    "pubsub-listener")

To publish, obviously:

    (with-connection ()
      (red:publish "foo" "test"))


## Pipelining example

For better performance Redis allows to pipeline commands
and delay receiving results until the end and do that in batch afterwards.
To support that there's `with-pipelining` macro.
Compare execution times in the following examples
(with pipelining and without: 6.567 secs vs. 2023.924 secs!):

    (let ((names (let (acc)
                   (dotimes (i 1000 (nreverse acc))
                     (push (format nil "n~a" i) acc))))
          (vals  (let (big-acc)
                   (dotimes (i 1000 (nreverse big-acc))
                     (let (acc)
                       (dotimes (i (random 100))
                         (push (list (random 10) (format nil "n~a" i)) acc))
                       (push (nreverse acc) big-acc))))))
      (time (redis:with-connection ()
              (redis:with-pipelining
                (loop :for k :in names :for val :in vals :do
                  (dolist (v val)
                    (apply #'red:zadd k v)))
                (red:zunionstore "result" (length names) names)
                (red:zrange "result" 0 -1))))

      ;; Evaluation took:
      ;;  6.567 seconds of real time
      ;;  3.900243 seconds of total run time (3.200200 user, 0.700043 system)

      (time (redis:with-connection ()
              (loop :for k :in names :for val :in vals :do
                (dolist (v val)
                  (apply #'red:zadd k v)))
              (red:zunionstore "result" (length names) names)
              (red:zrange "result" 0 -1))))

      ;; Evaluation took:
      ;; 2023.924 seconds of real time
      ;; 3.560222 seconds of total run time (2.976186 user, 0.584036 system)

Note, that `with-pipelining` calls may nest.


## Internals

Generic functions `tell` and `expect` implement the Redis protocol
according to the [spec](http://redis.io/topics/protocol).
`tell` specifies how a request to Redis is formatted.
`expect` — how the response is handled.
The best way to implement another method on `expect` is usually with
`def-expect-method`, which arranges reading data from the socket
and provides a variable `reply`, which holds the decoded reply data
from the server with the initial character removed. For example:

    (def-expect-method :ok
      (assert (string= reply "OK"))
      reply)

Redis operations are defined as ordinary functions by `def-cmd`
for which only arguments and return type should be provided.
`def-cmd` prefixes all the defined functions' names with `*cmd-prefix*`,
which defaults to `'red`.
(Note, that setting of `*cmd-prefix*` will have its effects at compile time).
An example of command definition is given below:

    (def-cmd KEYS (pattern) :multi
      "Return all the keys matching the given pattern.")

See `commands.lisp` for all defined commands.


## Not implemented

- The following commands are not implemented,
  because they are not intended for use in client:
  `MONITOR`, `DEBUG OBJECT`, and `DEBUG SEGFAULT`.
- [Consistent hashing](http://en.wikipedia.org/wiki/Consistent_hashing)
  isn't built-in.  Actually, such thing is orthogonal to the functionality
  of this library and, probably, should be implemented in a separate library.
- Connection pooling is also not implemented, because in the presence of
  `with-persistent-connection` it is not so much needed.
  But there are use-cases for it, so it will probably be implemented in future
  releases.