# CL-REDIS — A fast and robust Common Lisp client for Redis
  (tested with Redis version 3.0.0 (2.9.104 to be precise))

## Usage

### Quickstart

1. Make sure a Redis server is running.
2. `(ql:quickload 'cl-redis)`
3. Connect to the server to the given host and port with
   `(redis:connect :host <host> :port <port>)`
   (`host` defaults to `127.0.0.1`, `port` — to `6379`).
4. Interact with the server using Redis commands from the `red` package.

```lisp
CL-USER> (red:ping)
"PONG"
```

5. Disconnect from the server with `(redis:disconnect)`.
6. Alternatively, wrap the whole interaction session in `with-connection` macro,
   which accepts the same arguments as `connect` does, opens a socket connection,
   executes the body of the macro with the current connection (`*connection*`)
   bound to this new connection, and ensures that the connection is closed
   afterwards.

### Available commands



### Code organization

The system provides 2 packages: `REDIS` and `RED`.  All the
functionality is available from the `REDIS` package.  Not to cause
symbol clashes, Redis commands are defined in this package with a
prefix (which defaults to `red-` and is set at compilation time).
The package `RED` is a syntactic sugar — it just provides the Redis
commands without a prefix.  So it is not intended to be imported to
avoid symbol conflicts with package `COMMON-LISP` — just use the
package-qualified symbol names: i.e. the same Redis command (for
instance `GET`) can be called as `RED-GET` (if you import the `REDIS` package)
or `RED:GET`.


## Installation

Available through [quicklisp](http://quicklisp.org/).

### Dependencies

- [usocket](http://common-lisp.net/project/usocket/)
- [flexi-streams](http://common-lisp.net/project/flexi-streams/)
- [rutils](http://github.com/vseloved/rutils)
- only for tests: [nuts](http://github.com/vseloved/nuts),
  [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads)


## Debugging and error recovery

If `*echo-p*` is `T`, all client-server communications will be
echoed to the stream `*echo-stream*`, which defaults to `*standard-output*`.

Error handling is mimicked after
[Postmodern](http://common-lisp.net/project/postmodern/).
In particular, whenever an error occurs that breaks the communication stream,
a condition of type `redis-connection-error` is signalled offering
a `:reconnect` restart.  If it is selected the whole Redis command will be
resent, if the reconnection attempt succeeds.
Furthermore, `connect` checks if a connection to Redis is already established,
and offers two restarts (`:leave` and `:replace`) if this is the case.

When the server respondes with an error reply
(i.e., a reply that starts with `-`),
a condition of type `redis-error-reply` is signalled.

There's also a high-level `with-persistent-connection` macro,
that tries to do the right thing™
(i.e. automatically reopen the connection once, if it is broken).


## Advanced usage

### PubSub

Since there's no special command to receive messages from Redis via PubSub
here's how you do it:

```lisp
(bt:make-thread (lambda ()
                  (with-connection ()
                    (red:subscribe "foo")
                    (loop :for msg := (expect :anything) :do
                      (print msg))))
                "pubsub-listener")
```

To publish, obviously:

```lisp
(with-connection ()
  (red:publish "foo" "test"))
```

### Pipelining

For better performance Redis allows to pipeline commands
and delay receiving results until the end,
and process them all in oine batch afterwards.
To support that there's `with-pipelining` macro.
Compare execution times in the following examples
(with pipelining and without: 6.567 secs vs. 2023.924 secs!):

```lisp
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
```

Note, that `with-pipelining` calls theoretically may nest,
but the results will only be available to the highest-level pipeline,
all the nested pipelines will return :PIPELINED.
So a warining is signalled in this situation.


## Internals

Generic functions `tell` and `expect` implement the Redis protocol
according to the [spec](http://redis.io/topics/protocol).
`tell` specifies how a request to Redis is formatted,
`expect` — how the response is handled.
The best way to implement another method on `expect` is usually with
`def-expect-method`, which arranges reading data from the socket
and provides a variable `reply`, which holds the decoded reply data
from the server with the initial character removed. For example:

```lisp
(def-expect-method :ok
  (assert (string= reply "OK"))
  reply)
```

Redis operations are defined as ordinary functions by `def-cmd`
for which only arguments and return type should be provided.
`def-cmd` prefixes all the defined functions' names with `*cmd-prefix*`,
which defaults to `'red`.
(Note, that setting of `*cmd-prefix*` will have its effects at compile time).
It also exports them from `REDIS` package,
and from `RED` package without the prefix.

An example of command definition is given below:

```lisp
(def-cmd KEYS (pattern) :multi
  "Return all the keys matching the given pattern.")
```

See `commands.lisp` for all defined commands.


## Not implemented

- The following commands are not implemented,
  because they are not intended for use in client:
  `MONITOR`, `DEBUG OBJECT`, and `DEBUG SEGFAULT`.
- Support for Unix domain sockets — planned
- [Consistent hashing](http://en.wikipedia.org/wiki/Consistent_hashing)
  isn't built-in.  Actually, such thing is orthogonal to the functionality
  of this library and, probably, should be implemented in a separate library.
- Connection pooling is also not implemented, because in the presence of
  `with-persistent-connection` it is actually not needed so much.
  Persistent connections are more simple, efficient and less error-prone
  for dedicated threads.  But there are other use-cases for pooling,
  so it will probably be implemented in future releases.


## Credits

The library is developed and maintained by Vsevolod Dyomkin
<vseloved@gmail.com>.

At the initial stages Alexandr Manzyuk <manzyuk@googlemail.com>
developed the connection handling code following the implementation in
[Postmodern](http://common-lisp.net/project/postmodern/). It was since
partially rewritten to accommodate more advanced connection handling
strategies, like persistent connection.


## License

MIT (See LICENSE file for details).
