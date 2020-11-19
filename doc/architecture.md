# Architecture

## Intro
The `deflow` macro enables writing procedures, called flows, which await events in the real world, and which may occur over long periods of time. They’re conceptually similar to asynchronous processes in Javascript and other languages, however, while async functions and threads execute within the context of a single continuously running computer, the execution of a longterm flow may pass between different CPUs, as the program pauses for external events for arbitrarily long time periods.

The macro implements a compiler which analyzes a body of code, determining places where execution suspends for an external event, partitioning it into functions (continuations) which execute instructions before or after a point where execution suspends. The compiler associates each continuation with a unique address.  

A runtime environment maintains a stack which contains records called stack frames which identify the next continuation to be executed (by storing its address), as well as other values, which will be discussed below. As execution proceeds, code inside continuations push new stack frames onto the stack, which act as pointers to other continuations to be executed next. A runtime loop pops frames from the stack, executing continuations until a special Suspend signal is received. We'll get into details in a moment. 

First, consider this extremely simple flow which greets a user, asks for their name, and produces a reply that includes the user's name and the boolean which determines how excited the bot is, provided at the beginning of the flow:

```clojure
(deflow greeting [excited?]
  (respond! "Hi. What is your name?")
  (let [name (listen!)] ;; listen! is a suspending operation
    (respond! (str "Hi, " name))
    (respond! (if excited? 
                  "It's super duper, duper, duper, duper, (breathes) duper, duper, duper, duper nice to meet you!" 
                  "Nice to meet you."))
    name)) ;; return the name of the user
```

The `listen!` function represents a point where execution pauses for input. The compiler recognizes `listen!` as a suspending expression - a point where execution pauses. Execution resumes when an external source provides a value - in this case a user providing their name. When execution resumes, the value is bound to the `name` variable.

First let's jump to the output of the compiler to get a flavor of what is going on. 

```clojure
#<Flow
 :name greeting
 :entry-point (fn [excited?] (flow/call [greeting 0] {:excited? excited?}))
 :continuations
 { #address[greeting 0]            (fn [{:keys [excited?]}]
                                     (respond! "Hi. What is your name?")
                                     (resume-at [#address[greeting 1 let 0 1],
                                                [excited?], name]
                                       (listen!)))
   #address[greeting 1 let 0 1]    (fn [{:keys [excited?, name]}]
                                     (respond! (str "Hi, " name))
                                     (respond! (if excited? 
                                                   "It's super duper, duper, duper, duper, (breathes) duper, duper, duper, duper nice to meet you!" 
                                                   "Nice to meet you."))
                                     name)}>
```

The deflow macro produces a Flow record which contains a map containing two functions (continuations). You can see how each function is associated with a unique address, how the original code body is split at the suspending operation `listen!` and how that expression is wrapped in a macro, `resume-at` which links it to the second continuation, providing both the bindings of the lexical context up to that point (`[excited?`]) and the binding for the value which will eventually be received by `listen!` (`name`). Also notice that the second continuation takes both the existing bindings and the new binding as an argument.

We'll discuss how the infrastructure produces this code and uses it to implement long running processes in the next section. For now, just imagine that the execution of these two functions can be separated by an arbitrarily long period of time and executed on different CPUs. This gives you a flavor for how longterm models complex user interactions as functions and allows them to be delivered using traditional scalable web architecture.  


## Architecture Overview

1. Breaking of code into continuations.

   It includes a compiler which partitions code bodies into separate functions, referred to here as continuations, which carry out pieces of the computation.
   
2. Definition of suspending operations.

   The compiler recognizes suspending expressions to determine where to partition code bodies; suspending expressions include expressions which suspend computation ("suspend operations") and expressions which may suspend computation. The latter includes, for example, conditional expressions which contain a suspend operation in one branch and a normal expression (e.g., normal function evaluation or symbol evaluation) in the other branch, or invocations of flows, which might return immediately or suspend before returning.  
   
3. Where code is partitioned. 

   The compiler arranges for suspending expressions to always appear at the end of continuations.
   
4. Symbolic addresses.

   The compiler generates unique symbolic addresses for code locations
   
5. A map of continuations.

   The compiler stores continuations in a data structure, called a `Flow`, where they can be looked up at runtime using a symbolic address
   
6. The Runtime Environment.

   The architecture includes a runtime environment that manages a FILO stack. At runtime, continuations add stack frames to the stack. 
  
7. Contents of Stack Frames.

   A stack frame is a 3-tuple consisting of (a) a symbolic address referencing a continuation where computation should resume, (b) variable bindings up to that point of the computation, and (c) an optional variable name that an externally provided value should be bound to, called the `result-key`. That variable will be bound as an argument to the continuation referenced by the symbolic address (a).
   
   ```clojure
   ;; simplified way of creating a stackframe record in Clojure
   (StackFrame. address bindings result-key)
   ```
   
8. Rule for Adding Stack Frames.

   The compiler adds a stack push operation before non-terminal suspending expressions, arranging for the data described in (7) to be incorporated into a new stack frame. The stack frame provides the data necessary to resume computation at the next continuation. Note that a stack push is not required when a suspending expression appears in a terminal position. This is explained in more detail in discussion of the main loop.

   Example of terminal versus non-terminal positions:
   ```clojure
   (deflow myflow []
      ;; first partition begins here
      (x)
      ;; stack frame push here
      (suspending-expression) ; non-terminal
   
      ;; second partition begins here
      (y))
   ```
   ```clojure
   (deflow myflow []
     ;; a single partition:
     (x)
     (y)
     (suspending-expression)) ; terminal - stack frame push not required
   ```
   Suspend operations (like `listen!`, described below) return a special `Suspend` signal which is recognized by the runtime environment. As stated earlier, the compiler arranges for these expressions to be the last expression in each continuation, thus continuations which suspend will return `Suspend` signals. The importance of this is developed in discussion of the main loop. 
   
9. The start! function.

   Execution of a flow is begun by invoking a `start!` function, passing the `Flow` data structure and any arguments. E.g.,
   
   ```clojure
   (start! greeting true) ;  returns a Run object
   ```
   
10. Starting.

    The `start!` function generates a StackFrame where the address is a special continuation which identifies the beginning of the flow, denoted by a well known address, such as `{flow-name}/0`. Bindings are generated from the arguments provided via `start!` to the flow. For example, the above expression starting the greeting flow produces a stack frame: `[greeting/0, {:excited? true}, nil]`. Remember, these values are the address, the bindings and the result-key. A binding for a stack is established, the initial frame is pushed onto the stack, and the main loop is invoked with a result-value of `nil` and process the result as described below. The result of the post-processing step is returned.

11. Operation of the main loop,
 
    The main loop is a function taking a single argument, the `result-value`.
    a. If the stack is not empty, pop the frame at the top of the stack (the top frame), otherwise exit, returning the `result-value`.
    b. If the result-key of the top frame is non nil, add the `result-value` to the bindings, storing it under the `result-key`. E.g., `(assoc bindings result-key result-value)` in Clojure. The bindings thus prepared are called "the result bindings".
    c. Retrieve the continuation identified by the address of the top frame and call it with the result bindings.
    d. The continuation will return either a `Suspend` signal or some other value, if the return value is not a `Suspend` signal, recursively call the main loop (i.e., GOTO step a) with the return value provided as the `result-value`.
    e. If a `Suspend` signal is received, exit the main loop, returning the signal. 
    
12. Processing the result of the main loop.

    As seen above, the main loop either returns a `Suspend` signal or a value. The former indicates that external input is needed to continue, whereas the latter indicates that the flow initiated by `start!` has completed. At this stage, the contents of the stack are saved to durable storage under a unique id in a structure referred to as a "Run". The client which initiated the process receives the Run, which contains the state of the computation, as well as other useful data which is described below (Responding to the client during the run). This post processing step returns a run.
  
13. Continuing.

    After a flow is started, it will usually transition into a suspended state while it waits for input. The `continue!` function is provided for this purpose. It is a function of two arguments: an id which references a run and an optional `result-value`. It retrieves the Run from durable storage, regenerating the stack. The main loop is then invoked with the `result-value` and processed as described above, and the result of the post-processing step is returned.
    
    ```clojure
    (continue! run-id result-value) ; returns a Run object
    ```
   
14. Responding to the client during the run.

    Since it is necessary to communicate with the client while a run is ongoing, some other mechanism must be used than the result, which is only available at the end of the run. To be precise, a response is generated by the initial `start!` call and each invokation of the `continue!` call.  There are many possible mechanisms for generating a response - setting values in a map as the run is ongoing, appending values to a list or simply setting a global value. In our implementation, we choose to append to a list, as this accurately captures order. A post-processing function could be used to convert the list into some other structure - a map, set, or other value.
  
15. Saving the Run to durable storage.

    A Run contains three potentially complex data fields which represent objects generated during the computation: the response, the bindings and the result. While scalar values and composite objects like sets, maps, lists and arrays composed of scalars are easy to store. Other types of objects require special treatment. For example, functions cannot be easily persisted. A programmer may bind an object like the Flow data structure, but it since the entire definition of this object resides in the program, it is unnecessary to commit it to storage. In addition, it contains functions, so the problem for persisting functions applies again to Flow objects. In addition, Runs representing other asynchronous processes may be referenced in the bindings, and their state may evolve from the time the bindings were persisted to durable storage. For this reason, a mechanism for "freezing" and "thawing" objects which respects the different requirements for different types of objects is required. We use an existing library, [nippy](https://github.com/ptaoussanis/nippy ), which can be extended with custom functions for different types of entities, which we briefly outline here: 
  
    A. Storage-based objects: if an object represents an entity backed by durable storage, it will be stored in a structure which wraps only the information needed to retrieve the entity. Thawing the object requires retrieving it from durable storage.
   
    B. Global definitions: if the system can determine an object is globally defined, it will be stored in a structure which wraps the global symbol. Thawing the object is a simple matter of derefrencing the global symbol.
  
    C. Functions: globally defined functions can be handled as per (B), but lexically scoped functions, anonymous functions and closures need special treatment by the compiler.      
    
16. Guarding against invalid continuation.

    By adding a `permit` value to the `Suspend` signal, and storing this value in the Run, a requirement can be created for the caller of `continue!`. This is useful for preventing a client from responding to an earlier part of the flow. The `listen!` function is modified to take an optional argument `:permit` which must be matched with a value provided to the `continue!` function. The simplest version of this would just be to match the values exactly, but more sophisticated matching is possible by including a function. For example, the permit could be a private key. The client could be provided a public key and required to encode some value (e.g., the result-value) with that key and provide it as the permit.
    
    ```clojure
    ;; inside the flow:
    ... (listen! :permit :foo)
    
    ;; client code somehow ends up calling this:
    (continue! run-id result-value permit) ; where permit = :foo
    ```
  
17. Timers and timeouts.

    By storing an expiry time and optional default value in a suspended Run, it is possible to both allow for timing out of external events and create a mechanism for implementing timers. The `listen!` operator is modified to take  optional `:expires` and `:default` arguments. These are returned as values inside the `Suspend` signal and stored in the run in the post-processing step. The system arranges to periodically check for expired runs, simply continuing them with the default value (and appropriate `permit`).
  
20. Ensuring consistency.

    Runs are optimistically locked before retrieval from storage. We include this as one of the states of the `:state` field, but it could also be implemented as a boolean field, which is set to `true` when a run is acquired from storage. Only unlocked runs can be acquired. When runs are saved back to storage after post-processing, the lock is released. 
 
21. Versioning.
 
    As the flows are expected to change over time, a mechanism for maintaining versions of old flows is required, especially given that flows are intended to be long running and interact with users. One simple approach is to require programmers to maintain old versions of flows in the code base. Flow versions can be identified by computing a hash on the AST of the input to the `deflow` macro. The addressing scheme could be modified to include a reference to the particular version (deflow hash), thus any stack frame will uniquely identify a particular continuation in stored in the flow structure. Repeated invocations of `deflow` would thus add continuations to the continuation map without worry of collisions.  The current flow could be distinguished in some way - explicitly by adding a parameter to the `deflow` macro or including a rule that the first invocation (or last) of `deflow` for a given name sets the entry-point function. Since the entry-point function is unique to a flow, that would ensure that it points at the appropriate continuation in the continuation map.  
     
    Facilities for checking whether a code base supports existing flows could be implemented by retrieving a unique set of addresses used in stacks of runs which have not completed. Since each address names the flow, the hash and the code point, it would be a simple matter of accessing flows and testing for the existence of a continuation at each address. Other methods might include saving versions to source control and arranging for those code bases to be retrieved dynamically. 
     
     
### Summary

The architecture includes the following elements:

  A. A compiler which partitions code bodies and inserts Stack Push operations in existing code

  B. Suspending operations which return Suspend signals at runtime

  C. Continuations stored at unique addresses

  D. Continuations which save new stack frames to the stack at runtime

  E. A stack composed of stack frames, each of which contains the address of a continuation, a map of lexical bindings, and an optional result-key
 
  F. A main loop function which continuously pops stack frames and evaluates continuations until a `Suspend` signal is received.
 
  G. A means of saving the stack to persistent storage

  H. A means of retrieving the stack from persistent storage and resuming the loop  

## Definitions
   * partition - block of code from a (deflow ...) expression
   * address - a Clojure record which identifies a location in a flow
   * continuation - Clojure function associated with an address that implements the partition. In practice, this also includes code which pushes and pops frames onto/off of the stack
   * listen - stop execution of a run due to encountering the listen! command with a flow. The state of the flow is saved to the stack and can be resumed when an event is received.
   * suspending expression - within a lexical body of code, an expression which represents a point at which execution may pause for an arbitrarily long period of time.  
   * stack frame - a Clojure record which contains information required for resuming a flow it contains an address which identifies the next continuation, plus any bindings
   * stack - a FIFO stack of thunks; the most recent represents the next continuation
   * run - a sequence of execution involving multiple runlets and the events that trigger them.        also the object stored in the database that tracks the run
   * runlet - a sequence of continuations which are invoked while handling an event, representing partitions from potentially many flows. A runlet begins when a flow is started or continued and ends when a listen! operation is encountered. 
   * child run - a run started by another run. Moral equivalent of a subprocess.
   * event - a real world event which causes the run to resume executing after being listening
   * redirect - relinquish control to a child run 
   * context - value provided to listen! which identifies the type of event which may continue the run. Sometimes referred to as the "listen context". Runs may redirect control to child runs until the child run leaves a particular context. For example, a run responding to a user may redirect to a child run to initiate an activity with them. At some point, the child run switches context from gathering input from the user to listening for some other real world event, like results from a lab. When the child flow's listen context

## Coordinating Runs

The system described so far explains how a long running flow can manage an interaction with a client. However, real world processes often involve coordinating interactions between multiple entities. I healthcare, a patient, their family members, healthcare providers and labs provide data necessary for guiding care of the patient. Interactions of the system with these other entities will often happen in parallel to an interaction with the patient. In addition, the patient may engage in several activities in the system, some of which may become blocked while waiting for other processes to complete (e.g., waiting for a lab result).

We introduce operators for managing runs within a flow: `start!` (shorthand: `!`) and the `redirect!` (shorthand `>>`) and `block!` (shorthand: `<!`) operators. Replaces the current run with the redirected run, and allows responses generated by the redirected run to also be passed to the client. Blocking pauses the current run until another run completes. 

#### Caching Runs in the run context 

Multiple runs may interact during a request. Runs may block and redirect to each other potentially several times during the servicing of a single request, so returning constantly to persistent storage would result in poor performance. A method for ensuring consistency while maintaining performance is to retrieve them all from storage using optimistic locking, and keep an index of these runs in the runtime cache. Since requests are expected to complete relatively quickly and since runs coordinate with real world entities or processes related to individuals, frequency of access is expected to be on very long time scales, so contention for runs is not expected.

### Run States

  * `:running` - Code is being executed by a CPU. The run is started in this state. The runstore does not reflect the current state of the run.
  * `:suspended` - Code is not running; the current state of the computation is fully captured in the RunStore. The run moves out of `:suspended` state when:
                     * an external event is received (which matches the Suspend params)   
                     * a child run started with the blocking operator `<!` completes
                     * a child run started by redirection `>>` blocks  
  * `:complete` - evaluation complete, indicates result field was set

### Run Return Modes

  * `:block` mode is initiated using the `block!` (aka  `<!`) operator. It takes a run as an argument and suspends until the run completes. Although you could achieve the same behavior by simply calling a flow directly from another flow, it is better to launch a separate run to handle inputs from a different type of process. But the most important aspect of `:block` mode is that it causes a run suspended by `:redirect` to resume, as described in the following.

  * `:redirect` mode is entered using the `>>` operator, which takes a run as an argument. We refer to it as the child run. The Suspend permit is the id of the child run, and when the child run enters a blocked state, it calls `continue!` with its own id as the permit and returns the parent run to the caller, thus resuming the parent run.

   ```(continue! parent-run-id :permit child-run-id :result child-run)```
   
### Ports: "Channels for Runs"

  The existing infrastructure provides the basis for another powerful mechanism of interprocess communication: Channels, which we refer to as "Ports" to distinguish from the related runtime concept of a channel. Ports are implemented as tables. 
  
  It is possible to `put` and `take` values from ports in a similar way as is done with [channels](https://clojure.org/news/2013/06/28/clojure-clore-async-channels ), and ports similarly may be buffered or unbuffered. 
  
  Ports are created using `(port {n})`, where n is an optional value indicating the size of the buffer. A port is an object stored in durable storage, and is associated with a unique id, the "port id". This is a guaranteed globally unique identifier. We use UUIDs. 
  
  In addition, buffered ports provide different modes: FILO, FIFO and prioritized, where a second ordering value is provided during the `put` to determine the order by which values are retrieved during `take`. Under the hood, all buffered ports are prioritized, where FILO and FIFO ports take time or its negation as an implicit ordering value.
  
  Blocking takes from ports are done with the suspending operator `<_!!` which takes a port as an argument. If no values are in the port queue, the operator returns a suspend with a permit value equal to the port id. If a value is available, it returns immediately, return it. Since it is a suspending expression, the compiler has ensured it ends the continuation, so this value appears as a result-value in the main loop. 
  
  If the queue is empty, blocking take causes the current run id to be added to a many-to-many mapping in durable storage of run ids to port ids, the PortTakers table. This represents runs which are waiting on results from the port. 
  
  Similarly, a blocking put (e.g., `(_>!! port value {priority})` causes the calling Run to suspend if no runs are associated with the port in the port takers map and the port isn't associated with a queue or the queue is full. The run id and value are stored in a durable map, the PortPutters table, which maps a port to runs which are waiting to put values in the port.   

  
## Web Implementation

An obvious use case is to provide the longterm service over a web endpoint. The implementation is simply involves mapping web endpoints to the high level functions described above. It's a trivial exercise to imagine an implementation for websockets, and other transport protocols.

### Start run
```
POST /runs/{name-of-run}
Body: arguments
```

### Continue run
```
POST /runs/{run-id}
Body: 
{ "permit": permit // optional
  "result": {any-value} // optional
```

### Get run
```
GET /runs/{run-id}
```