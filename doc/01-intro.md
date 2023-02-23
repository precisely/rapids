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

Pools are analagous to channels in Clojure and Go. Each pool has a buffer which allows it to accept values without blocking the caller. By default, pools are created with a buffer of size zero. In the following discussion, "blocking" means causing a run to suspend. Runs are referred to as sources or sinks, depending on whether they are putting values into a pool or taking values out.  

##### Single pool functions: put-in! and take-out!

The `put-in!` function puts a value in a pool, specifically into the pool's buffer, a FILO queue. If the buffer is already at capacity, the calling run (the source) is blocked and the source is recorded in the pool's "sources" FILO queue. For example, a pool with a buffer of size 1 has a single slot, which is initially empty. A call to `put-in!` puts a value in the slot and does not block the caller. A second call to `put-in!` places a value in the buffer, but because the buffer is already at capacity, it blocks the caller. 

The `take-out!` function takes a value out of a pool's buffer queue. If the buffer contains values, the oldest one is returned, otherwise the calling run (the sink) is blocked and is recorded in a different FILO queue called the "sinks". If the pool contains sources, one is removed (the oldest, since it is FILO) and resumed. 

Note that a buffer with size>0 decouples sources and sinks.  For example, when a buffer has one empty slot and a source `S1` places a value `V1` in the buffer, `S1` does not block and is not placed in the sources queue. A second source `S2` putting a value `V2` in the buffer will block (and be added to the sources queue). Thus, when a sink retrieves `V1`, `take-out!` will cause `S2` to resume.

Similarly, if a sink attempts `take-out!` on a pool which contains no value, it will block until a value is put into the pool, unless a default value is provided.

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

Certain actions need to be performed only after a runlet has completed successfully. This is done using the `defer` function. It takes a nullary function which performs actions. Any return value is ignored. It is typically performs side effects which should be performed only after a runlet has completed successfully without error. For example, sending a notification to a user:

```clojure
(defer (fn [] (notify-user user-id "This is the message"))
```

### Interrupting runs

