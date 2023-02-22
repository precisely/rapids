# Introduction to Rapids

Rapids makes it easy to create sophisticated user interaction flows - for example chatbots or intelligent assistants, personalized onboarding experiences, or situations where multiple users need to be coordinated. The key idea is to represent human computer interactions as functions. Rapids calls these interaction functions "flows".

## Defining user interaction functions (flows)

Unlike regular CPU-bound functions, a flow may pause for arbitrarily long periods while it  waits for a human (or other external entity) to complete a task and possibly enter some data. Flows are defined using the `deflow` macro, which is analogous to Clojure's `defn`. The argument lists look the same:

```clojure
(defn [name docstring? & sigs] ...)
(deflow [name docstring? & sigs] ...)
```

The code bodies inside `deflow` can include most Clojure expressions, such as function calls, literals of all kinds, special operators, macros and Java interop. `deflow` bodies permit using some special operators. Flows are typically used in the context of building web applications that need to manage complex user interactions state.

Here's a highly repetitive chatbot that keeps greeting people by name:
```clojure
(deflow greeting-bot []
  (loop []
    (output! "What is your name?")
    (let [user-name (input!)]
      (when (not= user-name "stop")
        (input! (str "Hello, " user-name))
        (recur))))) 
```

### Getting input and producing output 

### Starting and continuing a run

### Anonymous flows and higher order flows

### Setting time outs

### Indexing and finding runs

### Coordinating runs

Two main methods exist for coordinating runs: blocking and pools. 

#### Blocking

```clojure
(start! lab-order [:blood-work])
(block! lab-order) ; returns when lab-order state => :complete
```

The `block!` operator takes a single argument, a run called the blocking run, and suspends the calling run until the blocking run completes. It returns the `:result` of the blocking run. Blocking is easy to use and conceptually simple to understand, but has relatively limited uses.

#### Communicating Sequential Processes

```clojure
(->pool) ; a pool with buffer size 0
(->pool 2) ; a pool with buffer size 2
(put-in! pool :foo) ; puts :foo in pool
(take-out! pool :default-val) ; takes a value out of pool or returns :default-val 
```

Rapids provides [CSP](https://en.wikipedia.org/wiki/Communicating_sequential_processes) style coordination, similar to what goroutines and channels enable in Clojure, Go and other languages. 

Pools are analagous to channels in Clojure and Go. A pool may have a buffer which allows it to accept values without blocking the caller.

##### Pool basic operators: put-in! and take-out!

The `put-in!` function puts a value in a pool, blocking the calling run if the buffer is full. The `take-out!` function takes a value out of a pool. If a run was blocked while putting a value into the pool, it is resumed. The earliest blocked run is always the one resumed. Similarly, if a run attempts `take-out!` on a pool which contains no value or blocked run, it will block until a value is put into the pool. If a pool has space in its buffer, a `put-in!` operation will not block. By default, the pool creation function `(->pool)` returns a pool with a buffer size of zero. Such a pool strictly synchronizes the source and destination runs. The `take-out!` function also takes an optional default value, which if provided guarantees that it never blocks. The default value is returned if the pool is empty.

##### Multipool coordination: take-any! and take-case!

```clojure
(take-any! [p1 p2 p3] :default-value)
```

The `take-any!` function allows waiting on many pools until the first one has a value. This function takes a sequence of pools as its first argument and an optional default value. If one of the pools contains a value, it returns a two tuple `[i v]` where `i` is the index of the pool which contains a value and `v` is the value. `take-any!` suspends the run if no default is provided and none of the pools has a value. It resumes when another run calls `put-in!` on one of the pools.

The `take-case!` macro is a wrapper around `take-any!` which provides a convenient syntax for attending on a fixed number of pools. It takes a variable `v` which will contain the value produced by a pool. An optional default value may be provided.

```clojure
(take-case! v
  p1 (print "pool p1 => " v)
  p2 (print "pool p2 => " v)
  p3 (print "pool p3 => " v))

;; guarantee it doesn't suspend by providing a default:
(take-case! v
  p1 (print "pool p1 => " v)
  p2 (print "pool p2 => " v)
  p3 (print "pool p3 => " v)
  :my-default-value)
```

### Defering actions for later

### Interrupting runs

