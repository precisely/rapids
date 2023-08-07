# Introduction to Rapids

Rapids makes it easy to create sophisticated user interaction flows. You can use Rapids to create long running structured interactions with AIs, personalized onboarding experiences, or situations where multiple users need to be coordinated. The key idea is to represent human computer interactions as functions. Rapids calls these "flows".

Flows are intended as a replacement for finite state machines. FSMs tend to be difficult to change, and difficult to scale. Control flow techniques used in programming languages (branching logic, loops, exception handling) are more compact, readable, and easier to test. Programs are more powerful, since they can have arbitrarily many states whereas FSMs must have a predefined set of states. 

Rapids was designed to enable long running functions (flows) which are accessible via a WEB API. Rapids is fully general, however, and connecting it to the Web should be regarded as an implementation detail. A separate project (https://github.com/precisely/pia-server) provides an example of how to do this. 

Rapids is an inversion of control technique applied to API programming. Instead of writing programs which respond to API calls, you write a program which conceptually reads and writes values from the API during the course of its execution. This allows thinking of user experiences as *programs* rather than scattering the logic of user experiences all over the infrastructure of a web site (in models, views, controllers, queues, etc). 

## Defining user interaction functions (flows)

Unlike regular CPU-bound functions, a flow may pause for arbitrarily long periods while it waits for a human (or other external entity) to complete a task and possibly provide some data. Flows are defined using the `deflow` macro, which is analogous to Clojure's `defn`. The argument lists look the same:

```clojure
(defn [name docstring? & sigs] ...)
(deflow [name docstring? & sigs] ...)
```

The code bodies inside `deflow` can include most Clojure expressions, such as function calls, literals of all kinds, special operators, macros and Java interop. `deflow` bodies permit using some special operators. Flows are typically used in the context of building web applications that need to manage complex user interactions state.

Here's a highly repetitive chatbot that keeps greeting people by name:
```clojure
(deflow greeting-bot
  "A bot that keeps asking your name"
  []
  (loop []
    (output! "What is your name?") 
    (let [user-name (input!)]
      (when (not= user-name "stop")
        (input! (str "Hello, " user-name))
        (recur))))) 
```

A `Run` is roughly the Rapids-equivalent of a Java thread or a Unix process. A `Run` is created from a flow using `start!`:

```clojure
(start! flow-name args [:index metadata]) ;  returns a new Run instance with a unique `:id` value

;; E.g.,
(start! greeting-bot [] :index {:user-id user-id}) 
```

The `start!` function provides arguments to the flow and allows setting metadata (the index). Most importantly, the `start!` function initiates the *run loop*, a function which manages the execution of the flow. This function is typically invoked inside an API endpoint for creating run objects. E.g., `POST /runs`. 

### Getting input and producing output 

The `input!` (aka `<*`) and `output!` (aka `>*`) operators get data from and send data to the outside world (symbolized by the `*`). You can think of them as "Web STDIN" and "Web STDOUT". When input is requested, the run loop is halted and the `Run` is put into a suspended state. The `continue!` function allows a client to provide a value that will be returned by `(<*)` within the body of the flow. The `continue!` function is typically invoked by the handler of an API endpoint. E.g., `POST /runs/{id}`. From a RESTful perspective, the `Run` is a document created by `start!`, and `continue!` mutates the document.

The programmer "writes" to the API by using `(>*)`. This operator writes its arguments to an append-only array that is saved in the `:output` field of the current `Run` instance. The output can be any kind of data. However it is typically a value that can be rendered as JSON, and so can be returned in a standard API call. Note, the output array is normally reset (set to a zero-length array) when `continue!` is invoked.

```clojure
(<*) ; shorthand for (input!)
(>*) ; shorthand for (output!)
```

When requesting input, it is possible to provide a time out and default value (described below). Note how this makes it trivial to create timed events (e.g., like for a marketing funnel).

```clojure
(<* :expires (-> 3 days from-now) :default "This string will be returned")
```
It is also possible to provide a test the client must satisfy by providing the `permit` parameter. This can be a value or a function. When provided, a subsequent `continue!` call must provide a valid permit value. Typically, the permit is provided to the client in a previous output or by some other method.

```clojure
(<* :permit "this-is-the-permit-value")
(<* :permit (fn [x] (= x "this-is-the-permit-value"))
```

Convenience functions may combine output and input forms into a single expression that generates a user interface element. E.g.,

```clojure
;; example use of a <*form flow that combines 
(let [result (<*form [(label "What did you think?")
                      (choice :rating [:great :ok :bad])])]
   (if (-> result :rating (= :great))
      (>* (text "Glad to hear that"))))
```
Note that the output generated by Rapids is entirely up to the programmer. Typically, a front-end library interprets the `:output` of a `Run` and generates appropriate UI which 

### Continuing a run

When an input expression is encountered during execution of a flow, Rapids saves the state of the stack. The flow can be continued by calling `continue!`. This function retrieves the `Run` from persistent storage and restarts the run loop, providing an input value to it.

```clojure
(continue! run-id & {:keys [input permit preserve-output]})

(continue! greeting-bot-run-id :input "Bob") ; normally "Bob" would be a value provided by a user
```

The `preserve-output` param is a boolean that if truthy prevents the `:output` array from being cleared. The `permit` param if provided is a test

### Killing a run

Sometimes, it's necessary to force a run to stop, just like you might with a Unix process. In most cases in the API, you can provide a `Run` instance or the `:id`.

`(kill! run)`

### Higher order flow programming

Flows can be defined anonymously similar to functions.
```clojure
(flow [a] (<*)) 
```
Use `fcall` and `fapply` to invoke anonymous flows or named flows:

```clojure
(deflow foo []
  (let [f (flow [a b c] (list a b c))]
    (fcall f 1 2 3) ;=> (1 2 3)
    (fapply f 1 2 [3]) ;=> (1 2 3)
    (fapply f 1 [2 3]) ;=> (1 2 3)
    (fapply f [1 2 3]) ;=> (1 2 3) 
```

Higher order flows analogous to functional counterparts like `map`, `reduce`, etc can be built   `fapply` and `fcall`, but are not available in the Rapids library.

### Timeouts and delays

Flows and suspending functions often take an `expires` argument. This is a timestamp that tells the system when to automatically resume the operations. If the operation returns a value, a `default` argument can be provided. 

Delays can be implemented using timeouts. When a timeout expires, the default value will be returned.

```clojure
(<* :expires (-> 3 days from-now))
(<* :expires (-> 3 days from-now) :default 123) ; in 3 days, this will return 123 
```

To ensure that delays are not triggered by inadvertent or intentional `continue!` calls, simply add a permit that cannot be provided or guessed.

```clojure
(<* :expires (-> 3 days from-now) :permit (uuid))
(<* :expires (-> 3 days from-now) :permit :mypackage/secret-delay-permit) 
```

### Indexing and finding runs

Runs contain a hash table called the index which can be used to reflect state. Unlike the output, the index is not cleared at the beginning of each runlet. Rapids database adapters ensure that the index is efficiently queryable. Avoid using the index as a set of global variables.

```clojure
(set-index! :foo 1) ; set the :foo key in the index in the current run
(set-index! run :bar 2) ; set :bar in the index of a provided run
(-> run :index :foo) ; returns 1
(current-run :index :foo) ; shortcut for accessing an index value within the current run
```

Runs can be retrieved from storage using `get-run` or `find-runs`. Find run enables searching for runs using the index.

```clojure
(get-run run-id) 
(find-runs [[[:index :patient-id] :eq patient-id]] :limit 3) ; see function doc for details
```

### Coordinating runs

Two main methods exist for coordinating runs: waiting for runs to complete and by data exchange, using pools. 

#### Waiting for runs to complete

```clojure
(start! lab-order [:blood-work])
(wait-for! lab-order) ; returns when lab-order state => :complete, returns lab-order result
```

The `wait-for!`  takes a run called the blocking run, and suspends the calling run until the blocking run completes. It returns the `:result` of the blocking run. Blocking is easy to use and conceptually simple to understand, but is limited to cases which do not require updates while a run is in `:running` state. This operator also takes optional `default` and `expires` arguments:

```clojure
(wait-for! run :my-default) ; returns immediately with :my-default if run isn't complete
(wait-for! run :my-default (-> 5 days from-now)) ; returns with :my-default in 5 days if run isn't yet complete
```
Similar to `wait-for!`, `wait-for-any!` enables waiting on multiple runs. It takes a vector of runs, and optional `default` and `expiry` arguments. This function returns a vector, `[index result]` , representing the index of the run which completed and the result it returned.

The `wait-cases!` is a wrapper around `wait-for-any!`, and provides a readable style for pairing expressions to be executed with a set of runs:

```clojure
(wait-cases! [result 
              :default :foo                     ; optional
              :expires (-> 3 days from-now)     ; optional
              :break #(= (count %) 3))]         ; optional
  r1 (list :run1 result)           ; result = value returned by r1
  r2 (print :run2 result)          ; result = value returned by r2
  ...
  :default (list :default result)) ; result will be :foo
```
See the function documentation for how to provide a default and expiry time.

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
(take-any! pools default? expiry?) => [index value]

(take-any! [p1 p2 p3]) ; suspends until one of the pools provides a value
(take-any! [p1 p2 p3] :default-value) ; returns :default-value if pools are empty
(take-any! [p1 p2 p3] :default-value (-> 5 days from-now)) ; waits on pools for 5 days
```

The `take-any!` function allows waiting on many pools until the first one has a value. This function takes a sequence of pools as its first argument and an optional default value. If one of the pools contains a value, it returns a two tuple `[i v]` where `i` is the index of the pool which contains a value and `v` is the value. `take-any!` suspends the run if no default is provided and none of the pools has a value. It resumes when another run calls `put-in!` on one of the pools.

The `take-case!` macro is a wrapper around `take-any!` which provides a convenient syntax for attending on a fixed number of pools. It takes a variable `v` which will contain the value produced by a pool. An optional default value may be provided.

```clojure
(take-case! v
  p1 (print "pool p1 => " v)
  p2 (print "pool p2 => " v)
  p3 (print "pool p3 => " v))

 guarantee it doesn't suspend by providing a default:
(take-case! [v :my-default]
  p1 (print "pool p1 => " v)
  p2 (print "pool p2 => " v)
  p3 (print "pool p3 => " v))
  
 guarantee it suspends for up to 5 days, then returns a default
(take-case! [v :my-default (-> 5 days from-now)]
  p1 (print "pool p1 => " v)
  p2 (print "pool p2 => " v)
  p3 (print "pool p3 => " v))
```

### Defering actions for later

Certain actions need to be performed only after a runlet has completed successfully. This is done using the `defer` function. It takes a nullary function which performs actions. Any return value is ignored. It is typically performs side effects which should be performed only after a runlet has completed successfully without error. For example, sending a notification to a user:

```clojure
(defer (fn [] (notify-user user-id "This is the message"))
```

### Interrupting runs

Rapids provides a means to interrupt running processes, handle those interruptions and restart execution at pre-specified code points. Interruptions are issued while a run is suspended at the application level, using the `interrupt!` top-level function. An interruption provides a name which is used to find handlers for that interruption type. When `interrupt!` is called, the run is put into a special `:interrupted` state for the duration of the handler - until it completes or invokes a restart.

#### The attempt macro

The `attempt` macro provides `handle` and `finally` internal forms, and a context in which restartable expressions may appear. The `restartable` form names a point in the code, wrapping a form. It allows execution to restart at that point using the `restart` method. Restarts are typically invoked within handle clauses.  

```clojure
 (attempt
   (let [dosage (restartable (calculate-dosage)
                  (:set-dosage [data] ...flow-body)
                  (:reinterview-patient 
                     "redo the dosage after interviewing the patient" ; :doc field of :metadata
                     {:interactive true} ; this is the metadata 
                     [message interview-parameters] ...)]
      (advise-patient-on-dosage dosage)
      (do-other-stuff)

      ; define multiple restarts within an attempt body
      (restartable (measure-inr-level..)
         (:retry [] (measure-inr-level))  equivalent to {:name :retry :do (flow [] get-cholesterol-level)})
         (:recompute "recompute the inr level" {:my-meta-data 123} [data] (recalculate-with data)))

    handlers - can run some code, and can either invoke a retrace or a recovery
                 note that during an interruption, the caller with the interruption ID
                 has control; the run is outputing to that caller.

   (handle :abort [_]   e.g., returning a different value
      (>* "Hello, doctor, I am aborting this dosing procedure")
      nil) ; return nil from this attempt block

   (handle :warfarin-sensitivity-change [{dosage :dosage}]   e.g., retracing to an earlier step
      (>* (str "Hello, doctor, I will reset the dose to " dosage " as you requested")
      (restart :set-dosage dosage) ; retraces are defined in the attempt

  
   (handle true i  catch any Interruption - demonstrates handling an arbitrary process
      (>* "I'm unable to determine what to do next. Please select one of the choices.")
      (>* (generate-choices-from-restarts (concat )))
   (finally ...))
```

#### List available interruption handlers

```clojure
(list-interruption-handlers) ; => sequence representing innermost to outermost interruption handlers
```

#### List available restarts

```clojure
(list-restarts)
```
