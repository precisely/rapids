# Rapids Architecture

## Intro
  The `deflow` macro enables writing procedures, called flows, which await events in the real world, and which may occur over long periods of time. They’re conceptually similar to asynchronous processes in Javascript and other languages, however, while async functions and threads execute within the context of a single continuously running computer, the execution of a rapids flow may pass between different CPUs, as the program pauses for external events for arbitrarily long time periods.

  The macro implements a compiler which analyzes a body of code, determining places where execution suspends for an external event, partitioning it into functions (partition functions) which execute instructions before or after a point where execution suspends. The compiler associates each partition function with a unique address.

  A runtime environment maintains a stack which contains records called stack frames which identify the next partition function to be executed (by storing its address), as well as other values, which will be discussed below. As execution proceeds, code inside partition functions push new stack frames onto the stack, which act as pointers to other partition functions to be executed next. A runtime loop pops frames from the stack, executing partition functions until a special Suspend signal is received. We'll get into details in a moment.

  First, consider this extremely simple flow which asks the user for their name and greets them, either enthusiastically or not:

```clojure
(deflow greeting [excited?]
  (output! "Hi. What is your name?")
  (let [name (input!)] ;; input! is a suspending operation

     ;; in principle, the code after this point might execute on a different computer
    (output! (str "Hi, " name))
    (output! (if excited?
                  "It's super duper, duper, duper, duper, duper, duper, duper, duper, (breathes) duper, duper, duper, duper, duper, duper, duper nice to meet you!"
                  "Nice to meet you."))
    name)) ;; return the name of the user
```

  The `input!` function represents a point where execution pauses for input. The compiler recognizes `input!` as a suspending expression - a point where execution pauses. Execution resumes when the system receives data from an external event (e.g., the user provides their name to a client, and the client generates an HTTP `POST` request on the rapids server). When execution resumes, the user's input (the "data") is bound to the `name` variable (the "input-key"). Note that bindings (in this case just one symbol, `excited?`) are preserved in the second half of the code body.

First let's jump to the output of the macro to get a flavor of what is going on. This is a slightly simplified version of what the macro produces:

```clojure
#Flow {
 :name greeting
 :entry-point (fn [excited?] (flow/call [greeting 0] {:excited? excited?}))
 :partition-fns
 { #address[greeting 0]            (fn [{:keys [excited?]}]
                                     (output! "Hi. What is your name?")
                                     (resume-at [#address[greeting 1 let 0 1],
                                                [excited?], name]
                                       (input!)))
   #address[greeting 1 let 0 1]    (fn [{:keys [excited?, name]}]
                                     (output! (str "Hi, " name))
                                     (output! (if excited?
                                                   "It's super duper, duper nice to meet you!"
                                                   "Nice to meet you."))
                                     name)}}
```

  The deflow macro produces a Flow record which contains a map with two functions (partition functions). You can see how each function is associated with a unique address, how the original code body is split at the suspending operation `input!` and how that expression is wrapped in a macro, `resume-at` which links it to the second partition function (explained below), providing both the symbols of the lexical context up to that point (`[excited?]`) and the symbol for the data (called the "data key") which will eventually be received by `input!` (in this case, `name`). Also notice that the second partition function takes a parameter list comprised of the previous partition function's lexical symbols and the data key.

  We'll discuss how the infrastructure produces this code and uses it to implement long running processes in the next section. For now, just imagine that the execution of these two functions can be separated by an arbitrarily long period of time and executed on different CPUs. This gives you a flavor for how rapids models complex user interactions as functions and allows them to be delivered using traditional scalable web architecture.

  After the architecture overview, we describe how clients might interact with the service to make use of long term flows. At the end, we mention advanced topics which describe operators which allow coordinating clients of the service with each other.

## Architecture

### Compiler and Runtime

1. Breaking of code into partition functions.

   It includes a compiler which partitions code bodies into separate functions, referred to here as partition functions, which carry out pieces of the computation.

2. Definition of suspending operations.

  Suspending expressions include expressions which suspend computation ("suspending operations", such as the `(input!)` call above) and expressions which may suspend computation. The latter includes, for example, conditional expressions which contain a suspend operation in one branch and a non-suspending expression in the other branch, or invocations of flows, which themselves might return immediately or suspend before returning due to internal branching logic, or any expression which contains subexpressions which may suspend.
   
  Notice that suspending *expressions* are not guaranteed to suspend, while suspending *operations* always suspend. Suspending expressions are simply expressions which may result in a suspending operation. 

3. Where code is partitioned.

  Sequences of expressions are split (partitioned) immediately following suspending expressions, and nested expressions containing suspending expressions are decomposed to evaluation order to allow invokation of suspending expressions in a separate partition. The partitioner guarantees that each suspending expression appears as a terminal expression within a partition.  Suspending expressions return a special value, called the Suspend Signal, indicating that a runlet has completed. Since suspending expressions are always the final expressions in partition functions, it follows that partition functions return Suspend Signals when suspending expressions are executed.

4. Symbolic addresses.

  The compiler generates unique symbolic addresses for code locations

5. A map of partition functions.

  The compiler stores partition functions in a data structure, called a `Flow`, where they can be looked up at runtime using a symbolic address

6. The Runtime Environment.

  The architecture includes a runtime environment that manages a FILO stack. At runtime, partition functions add stack frames to the stack.

7. Contents of Stack Frames.

  A stack frame is a 3-tuple consisting of (a) a symbolic address referencing a partition function where computation should resume, (b) variable bindings up to that point of the computation, and (c) an optional variable name that an externally provided value should be bound to, called the `input-key`. That variable will be bound as an argument to the partition function referenced by the symbolic address (a).

```clojure
;; simplified way of creating a stackframe record in Clojure
(StackFrame. address bindings input-key)
```

8. Rule for pushing stack frames.

  The compiler always adds a stack push operation before non-terminal suspending expressions, arranging for the data described in (7) to be incorporated into a new stack frame. The stack frame provides the data necessary to resume execution at the next partition function. Note that a stack push is not required when a suspending expression appears in a terminal position. This is explained in more detail in discussion of the main loop.

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
  Suspend operations (like `input!`, described below) return a special `Suspend` signal which is recognized by the runtime environment. As stated earlier, the compiler arranges for these expressions to be the last expression in each partition function, thus partition functions which suspend will return `Suspend` signals. The importance of this is developed in discussion of the main loop.

9. The start! function.

  Execution of a flow is begun by invoking the `start!` function, passing the `Flow` data structure and any arguments. E.g.,

```clojure
(start! greeting [true]) 
```

10. Starting.

  The `start!` function generates a StackFrame where the address is the partition function which identifies the beginning of the flow. Bindings are generated from the arguments provided via `start!` to the flow. For example, the above expression starting the greeting flow produces a stack frame: `(StackFrame. #address<greeting/0>, {:excited? true}, nil)`.

  Remember, these values are the address, the bindings and the input-key. The `start!`  function allocates an empty array for the stack, the initial frame is pushed onto the stack, and the main loop is invoked with a `nil` argument. The result of the main loop is processed as described below, saving the state of computation to durable storage, and an object which allows resuming the computation is returned.

11. Operation of the main loop.

  The main loop is a function taking a single argument, the `data`, which implements the following algorithm:

  a. If the stack is not empty, pop the frame at the top of the stack (the top frame), otherwise exit, returning the `data`.

  b. If the input-key of the top frame is non nil, add the `data` to the bindings, associating it with the `input-key`. E.g., `(assoc bindings input-key data)` in Clojure. The bindings thus prepared are called "the continue bindings".

  c. Retrieve the partition function identified by the address of the top frame and call it with the continue bindings.

  d. The partition function will return either a `Suspend` signal or some other value, if the return value is not a `Suspend` signal, recursively call the main loop (i.e., GOTO step a) passing the return value as the argument. (The return value becomes the `data` of the next iteration of the loop).

  e. If a `Suspend` signal is received, exit the main loop, returning the signal.

12. Processing the result of the main loop.

  As seen above, the main loop either returns a `Suspend` signal or a value. The former indicates that external input is needed to continue, whereas the latter indicates that the flow initiated by `start!` has completed. At this stage, the contents of the stack are saved to durable storage under a unique id in a structure referred to as a "Run". The client which initiated the process receives the Run, which contains the state of the computation, as well as other useful data which is described below ("outputing to the client during the run"). This post processing step returns a run.

13. Continuing.

  After a flow is started, it will usually transition into a suspended state while it waits for input. The `continue!` function is provided for this purpose. It is a function of two arguments: an id which references a run and an optional `data`. It retrieves the Run from durable storage, regenerating the stack (discussed below in ["Storing and regenerating state"](#storing-and-regenerating-state)). The main loop is then invoked with the `data` and processed as described above, and the result of the post-processing step is returned.

```clojure
(continue! run-id data) ; returns a Run object
```

14. outputing to the client during the run.

  Since it is necessary to communicate with the client while a run is ongoing, some other mechanism must be used than the result, which is only available at the end of the run. To be precise, a response is generated by the initial `start!` call and each invokation of the `continue!` call. There are many possible mechanisms for generating a response - setting values in a map as the run is ongoing, appending values to a list or simply setting a global value. In our implementation, we choose to append to a list, as this accurately captures order. A post-processing function could be used to convert the list into some other structure - a map, set, or other value.

```clojure
(deflow outputer []
  (output! "a")
  (output! "b")
  (output! {:echo (input!)})
  (output! "c")
  (output! "d"))
```

  The initial `start!` call creates a run and replies with a data structure representing a newly created Run object. The response thus far is reported:

```clojure
(start! outputer)
;; => #Run{:id "72278789-99b8-4626-aa5c-ef7997305cb1",
;;         :output ["a" "b"]},
;;         :state :suspended}
```

  When the run is continued, a new response vector is allocated and the next runlet is executed. Compare the response vector below with the one from the previous response:

```clojure
(continue "72278789-99b8-4626-aa5c-ef7997305cb1" "RESULTVALUE")
;; => #Run{:id "72278789-99b8-4626-aa5c-ef7997305cb1",
;;         :output ["RESULTVALUE" "c" "d"],
;;         :state :complete}
```

  The response vector can be thought of as a set of instructions to a particular client. A chatbot client would treat these as commands to create text bubbles, buttons and so on. A robot client might expect objects that represent operations for moving and sensing. A more traditional user interface might include primitives for displaying pages and making updates to those pages. Another type of client might be a lab which reports status of processing a sample, but doesn't expect much in the way of a response from the server.

<a name="storing-and-regenerating-state"></a>
15. Storing and regenerating state. 

  A Run contains three potentially complex data fields which represent objects generated during the computation: the response, the bindings and the result. While scalar values and composite objects like sets, maps, lists and arrays composed of scalars are easy to store. Other types of objects require special treatment. For example, functions cannot be easily persisted. A programmer may bind an object like the Flow data structure, but since the entire definition of this object resides in the program, committing it to durable storage would be wasteful. In addition, it contains functions, so the problem for persisting functions applies also to Flow objects. In addition, Runs representing other asynchronous processes may be referenced in the bindings, and their state may evolve from the time the bindings were persisted to durable storage. For this reason, a mechanism for "freezing" and "thawing" objects which respects the different requirements for different types of objects is required. We use an existing library, [nippy](https://github.com/ptaoussanis/nippy ), which can be extended with custom functions for different types of entities, which we briefly outline here:

  A. Storage-based objects: if an object represents an entity backed by durable storage, it will be stored in a structure which wraps only the information needed to retrieve the entity. Thawing the object requires retrieving it from durable storage.

  B. Global definitions: if the system can determine an object is globally defined, it will be stored in a structure which wraps the global symbol. Thawing the object is a simple matter of derefrencing the global symbol.

  C. Functions: globally defined functions can be handled as per (B), but lexically scoped functions, especially closures, need special treatment by the compiler. This is described below in ["Storing and regenerating closures"](#storing-and-regenerating-closures).

  Because of the simplicity of the design, many types of storage could be used to implement the service, including relational or document-oriented databases, or file systems. The nippy library referenced above is but one example of similar facilities available in many other languages.

<a name="storing-and-regenerating-closures"></a>

16. Storing and regenerating closures.

  Closures require special handling by the compiler. Persisting functions at runtime to a durable store is difficult - but also unnecessary. Closures, however, contain data that must be passed between partition functions. The compiler includes a partitioner that recognizes local function definitions. In Clojure, this is implemented by recognizing forms that begin with `fn` or `fn*`. When such forms are encountered, the compiler arranges for the function to be included in a vector associated with the flow object itself. The index of the local function in this array is called the _local function index_.

  The `(fn ...)` form is substituted with a record representing the closure, `Closure`. It is a three tuple containing (1) a reference to the flow, (2) the local function index and (3) the bindings up to that point. By analyzing the function body, it's possible to reduce the size of the object which must be stored in the closure to just the variables captured by the closure, however, since the storage system could also include a compression layer, this may not provide much additional value.

  Note that the `Closure` object contains only persistable data, and contains all of the information required to call the local function with the appropriate local bindings. By making this object callable (a simple matter of implementing the `IFn` interface in Clojure), it can be passed to other partition functions within the flow or even to other flows.  A similar technique can be used for implementing local flow definitions and treating them as first class objects.

17. Guarding against invalid partition function.

  By adding a `permit` value to the `Suspend` signal, and storing this value in the Run, a requirement can be created for the caller of `continue!`. This is useful for preventing a client from outputing to an earlier part of the flow. The `input!` function is modified to take an optional argument `:permit` which must be matched with a value provided to the `continue!` function. The simplest version of this would just be to match the values exactly, but more sophisticated matching is possible by including a function. For example, the permit could be a private key. The client could be provided a public key and required to encode some value (e.g., the data) with that key and provide it as the permit.

```clojure
;; inside the flow:
... (input! :permit :foo)

;; client code somehow ends up calling this:
(continue! run-id data permit) ; where permit = :foo
```

18. Timers and timeouts.

  By storing an expiry time and optional default value in a suspended Run, it is possible to both allow for timing out of external events and create a mechanism for implementing timers. The `input!` operator is modified to take optional `:expires` and `:default` arguments. These are returned as values inside the `Suspend` signal and stored in the run in the post-processing step.  The system arranges to periodically check for expired runs, simply continuing them with the default value (and the `permit` stored in the db). If a traditional database is used for durable storage, storing the expiry time in an indexed column associated with the Run is a simple way to discover runs which should be expired. If a filesystem is used for durable storage, a separate file-based index could be stored to achieve the same effect.

19. Ensuring consistency.

  Runs are optimistically locked before retrieval from storage. We include this as one of the states of the `:state` field, but it could also be implemented as a boolean field, which is set to `true` when a run is acquired from storage. Only unlocked runs can be acquired. When runs are saved back to storage after post-processing, the lock is released.

20. Versioning.

  As the flows are expected to change over time, a mechanism for maintaining versions of old flows is required, especially given that flows are intended to be long running and interact with users. One simple approach is to require programmers to maintain old versions of flows in the code base. Flow versions can be identified by computing a hash on the AST of the input to the `deflow` macro. The addressing scheme could be modified to include a reference to the particular version (deflow hash), thus any stack frame will uniquely identify a particular partition function in stored in the flow structure. Repeated invocations of `deflow` would thus add partition functions to the partition function map without worry of collisions.  The current flow could be distinguished in some way - explicitly by adding a parameter to the `deflow` macro or including a rule that the first invocation (or last) of `deflow` for a given name sets the entry-point function. Since the entry-point function is unique to a flow, that would ensure that it points at the appropriate partition function in the partition function map.

  Facilities for checking whether a code base supports existing flows could be implemented by retrieving a unique set of addresses used in stacks of runs which have not completed. Since each address names the flow, the hash and the code point, it would be a simple matter of accessing flows and testing for the existence of a partition function at each address. Other methods might include saving versions to source control and arranging for those code bases to be retrieved dynamically.

21. Mechanism of partitioning.

  The general principles of the compiler were described earlier. The compiler is composed of partitioning functions ("partitioners") which examine expressions and return values which are used to generate a flow's partition functions.

  All partitioners take 4 arguments (1) an expression, (2) the address of the expression, (3) the address of the current partition, (4) parameters (variables) defined up to this point in the computation. One partitioning function, `partition-body`, differs from others in that it takes a sequence of expressions rather than a single expression as its first argument.

  Partitioners return a 3-tuple consisting of (1) a `start` expression, (2) a partition set, and (3) a boolean indicating whether the expression is a suspending expression. The `start` is an expression which initiates a suspending computation. Ironically, the start expression always appears at the end of a partition function, according to the rules we outlined earlier. The partition set is a map of addresses to function definitions.

  The `deflow` macro takes three arguments: `name`,`params` and a variadic argument, `body`, the code body. It invokes `partition-body` on `body`, a sequence of expressions, which are processed by calling `partition-expr` on each one. On start, `deflow` it passes an address consisting of only the symbol being used to define the function.

  Partitioners each deal with a different class of expression. For example, one partitioning function deals with the special case of conditional expressions, another deals with functions, another deals with looping constructs, and so on.

  Partitioners generate new addresses for subexpressions by taking the input address and appending the current expression's operator, then appending the position of the subexpression within the current expression. For example, if the current address is `main/1`, and we are partitioning `(if (test) then else)`, the `(test)` subexpression address will be `main/1/if/0`. There are other approaches for generating unique addresses but this one is convenient because address locations are well defined.

  The partition algorithm works applies the following methodology to each type of expression. (1) it processes subexpressions in the order they would be evaluated. E.g., For function call expressions, the leftmost argument is processed first by calling `partition-expr`. (2) if the sub-expression is a suspending expression (detected by examing the third value of the returned tuple), the `start` expression is used in place of the original expression. (3) if a sub-expression *B* follows a suspending sub-expression *A*, then a stack frame is pushed before *A*'s `start`, with the address of *B*. E.g.,

```clojure
(do (A)
    (B)) ;; B-address
```
  becomes this pseudocode:

```clojure
(do (resume-at [{B-address} ...])
   (A)) ;; partition function ends here
```
  Note in this example, the `start` of `(A)` is just `(A)`, but in other situations, for example a function which takes suspending arguments, the suspending arguments must be evaluated before the actual function evaluation, which would happen in a separate partition function. E.g.,

```clojure
(A 100 (suspending-expression))
```

  produces `start` resembling this pseudocode:
```clojure
(let [A-arg-0 100]
  (resume-at [{continue-evaluating-A-call-address} {} A-arg-1]
    (suspending-expression)))
```

  A second partition function is generated that looks like the following:
```clojure
(fn [{:keys [A-arg-0, A-arg-1]}
  (A A-arg-0 A-arg-1)])
```

  The mechanism outlined here is imperfect because it creates more stack push operations than strictly necessary. For example, the following expression results in 3 partitions and 2 stack push operations.

```clojure
(do
  (A)
  (B (input!))
  (C))
```

  The first includes `(A)` and `(input!)`, then second includes an expression like `(B arg-0)` and the third includes `(C)`. In principle, this could have been reduced to 2 partitions and one stack push. The algorithm could be improved by adding the final partition address and body to the values returned by partitioners. That would allow a partitioner like `partition-body` to continue adding expressions to the final partition of a sub-expression without requiring a stack-push to continue executing a sequence of expressions after a suspending expression.

22. Partitioning function expressions.

  Eager evaluation function semantics mean involve a function operator followed by 0 or more arguments. Such expressions are evaluated by evaluating arguments first, left to right and passing the results in the same order to the function. E.g., in the following,

```clojure
(a (b) (c))
```

  `(b)` is evaluated before `(c)` and then function `a` is called with the results.

  The function partitioner partitions each argument, binding each result to a auto-generated parameter. When suspending operations are generated, a stack push is generated preceding the suspending expression's `start` expression, and the process continues in a second partition function until all arguments have thus been processed. The final expression receives the auto-generated parameters and applies the function (in this case, `a`) to the arguments.

23. Partitioning loops.

  It is possible to implement efficient long term looping semantics by checking whether a loop body includes a suspending operation or not. If it does not, the loop may be compiled normally using looping constructs of the underlying language. If it does, then the compiler introduces a new partition function at the start of the loop expression (the "loop partition function"), and replaces the recur expressions with a stack-pus, where the stack frame points to the address of the partition function containing the start of the loop expression. Alternatively, one could simply replace the `recur` expression with a call to the loop partition function. 
    
  The former mechanism generates a temporary Stack Frame which is immediately processed by the runtime loop. The latter avoids this so is marginally more performant, however, it consumes system stack which is not released until the loop is complete. In some situations, this could lead to stack overflows if, for example, a loop contains a suspending expression in side a conditional branch which is never encountered. If such a loop continues for a long time, each `recur` will consume more system stack in the latter method. 

24. Partitioning branching logic.

  A branching logic expression consists of `test`, `then` and `else` clauses. A branching logic expression which contains no suspending expressions is incorporated as is by the compiler. If the `test` clause is a suspending expression, then code is partitioned at the test expression. That is, the test expressions ends the current partition function, a stack frame is pushed with a new unique auto-generated variable for the `input-key`. The address will point to a new partition, the "branch partition" or "branch partition function" which contains a conditional expression which takes the data key variable as an argument and the start forms of the `then` and `else` clauses as arguments:

```clojure
;;; input form
(if (my-flow) ; my-flow is a suspending operation
   then-clause
   else-clause)
```

  becomes

```clojure
;; initial partition function ends with:
...
(resume-at [address-of-partition function-2 {..bindings} my-flow-test]
   (my-flow))

;; branch partition function looks like:
(fn [{:keys [... my-flow-test-result]}]
   (if my-flow-test-result
       then-start
       else-start))
```

25. Basic higher order flows.

  Two functions, `fcall` and `fapply` perform call and apply sematics on flows. The compiler recognizes `(fcall ..)` and `(fapply ...)` expressions as suspending expressions. The functions themselves simply perpare their arguments according to standard `call` and `apply` semantics, respectively, and pass the bindings to the entry point partition function of the flow object.

26. Advanced higher order flows.
    
  Higher order functions such as map, reduce and filter may be defined using the facilities described thus far. Thus, it is possible to define rapids map, reduce and filter flows using deflow:

E.g.,
```clojure
(deflow fmap [flow seq]
  (loop [[head & tail] seq
         result []]
    (let [new-result (conj result (fcall flow head))]
       (if (empty? tail)
           new-result
           (recur tail new-result)))))
```

27. Exception handling.

  Note: these ideas have been superceded by interruption handling. The current library does not implement exception handling as described below, but I'm preserving the notes for future reference:

  The compiler may provide a means for handling exceptions with a try partitioner. Try blocks might include suspending expressions. If the body inside the try block is non-suspending then the try block is included in the existing partition without modification, and the body inside the catch block is partitioned normally. However, if the try block includes a suspending operation, the compiler takes the following actions:

  A new partition is started at the try block. The catch clauses and finally (if provided) blocks are partitioned. Before partitioning, the catch clauses are transformed to implement the logic of a sequence of `instance?` tests on a special generated parameter which will hold the exception object on entry. I.e.,

```clojure
(try
  (suspending-expression)
  (catch ExceptionType1 e
    ...catch-code-block-1)
  (catch ExceptionType2 f
    ...catch-code-block-2)
  ...
)
```

  Is transformed as follows before being submitted to `partition-expr`. Assume `exc` is the parameter which will be bound to the exception object on input:
```clojure
(cond
  (instance? ExceptionType1 exc) (let [e exc] ...catch-code-block-1)
  (instance? ExceptionType2 exc) (let [f exc] ...catch-code-block-1)
  :else ::unhandled)
```

  Note that this block returns a special signal (here, represented as `::unhandled`). We'll see how this signal is used to determine whether

  The partitioner is called on the `catch` expression above and the `finally` block. The param vector will be the same one which are passed to the partitioner which processes the try block, with the addition of the `exc` param when partitioning the `catch` expression.

  We'll refer to the start addresses of each as the "catch address" and the "finally address", respectively. These addresses are recorded in the stack frame (the `resume-at` macro is modified to include extra parameters for these values). Alternatively, code is injected at the beginning of the try block to add these values to the current stack frame.

  The runtime implements exception handling as follows: a partition function execution function `execute-partition function` is added which takes four parameters, a set of bindings, a partition function address (code address), and optional catch and finally addresses:

```clojure
(execute-continuation bindings, code-address, catch-address, finally-address)
```

  This function executes the code continuation inside of a try-catch block (non-suspending code in the runtime), and ensures that the catch continuation is invoked if an exception is caught. It also ensures that the finally continuation is called irrespective of whether an exception was detected, thus mimicking traditional try/catch semantics. The catch and finally continuations are invoked by recursively calling `execute-continuation` with the catch or finally address passed as the code address, the same bindings, and nil provided for the catch parameter and finally bindings.

  When `execute-continuation` is called, the code continuation throws an exception and the catch address is `nil`, a process of unwinding the stack is begun. The stack is popped and if the stack frame has a catch address, the associated catch continuation is invoked recursively with `execute-continuation`. If the catch continuation returns any value other than the `::unhandled` signal, it indicates that the catch continuation handled the exception. The value returned by the catch handler is passed back to the main loop, resuming the computation.

28. Macroexpansion

  In languages which include macroexpansion, the partitioner should arrange to macroexpand expressions when it encounters macros and those macros are not handled explicitly by a partioning function.

#### Coordinating Runs

  The system described so far explains how a long running flow can manage an interaction with a client. However, real world processes often involve coordinating interactions between multiple entities. In healthcare, a patient, their family members, healthcare providers and labs provide data necessary for guiding care of the patient. Interactions of the system with these other entities will often happen in parallel to an interaction with the patient. In addition, the patient may engage in several activities in the system, some of which may become blocked while waiting for other processes to complete (e.g., waiting for a lab result).

  We introduce operators for managing runs within a flow: `start!` (shorthand: `!`) and the `redirect!` (shorthand `>>`) and `block!` (shorthand: `<!`) operators. Replaces the current run with the redirected run, and allows responses generated by the redirected run to also be passed to the client. Blocking pauses the current run until another run completes.

### Client Interaction

  Clients of this service perform three main actions: (1) starting a run, (2) continuing a run and (3) retrieving a run. Clients include any entity external to the system including end user devices like mobile phones and personal computers, internet-connected measuring devices, robots, servers of third party services, and so on. A REST API for interacting with the service is described below under "Web Implementation".

  In one use of the system, a client is a mobile phone app or web browser which displays a chatbot-like interface and queries the service for a starting flow. The responses are be objects which describe responses from a chatbot - text bubbles, buttons and other controls. The controls contain information which allows the client to continue interacting with the API and producing new responses. We can re-envision our greeting flow with this in mind:

```clojure
(deflow greeting [{:keys [excited?]}]
  (output! {:type :text, :text "Hi. What is your name?"})
  (output! {:type :text-input, :permit :name})
  (let [name (input! :permit :name)] ;; input! is a suspending operation

     ;; in principle, the code after this point might execute on a different computer
    (output! {:type :text, :text (str "Hi, " name)})
    (output! {:type :text, :text (if excited?
                  "It's super duper, duper, duper, duper, duper, duper, duper, duper, (breathes) duper, duper, duper, duper, duper, duper, duper nice to meet you!"
                  "Nice to meet you.")})
    name)) ;; return the name of the user
```

  The client calls this flow by issuing an HTTP POST to an endpoint like `/runs/greeting` with body `{"excited": true}` and application/json encoding. The server would process the request, looking up the `greeting` flow and calling `start!` with the argument, converting the JSON map to a Clojure-style one. The result would be a Run object, which would be returned as JSON as follows:

```json
{
   "id": "f3162328-97b9-4f67-a15a-c77f1d9647d5",
   "response": [
     {"type": "text", "text": "Hi. What is your name?"},
     {"type": "text-input", "permit": "name" }
   ],
   "state": "suspended"
}
```

  The client would use this information to display objects on the user's screen, a text bubble and a text input box with a button enabling them to send their response. When the user enters information and clicks send, the client performs a second HTTP POST to an endpoint like `runs/f3162328-97b9-4f67-a15a-c77f1d9647d5` with body `{"permit": "name", "data": "Bob"}`. The server would then output with:

```json
{
   "id": "f3162328-97b9-4f67-a15a-c77f1d9647d5",
   "response": [
     {"type": "text", "text": "Hi, Bob"}, 
     {"type": "text", 
      "text": "It's super duper, duper, duper, duper, duper, duper, duper, duper, (breathes) duper, duper, duper, duper, duper, duper, duper nice to meet you!" }],
   "state": "complete"
}
```

  Because the flow has ended, the state is marked as `"complete"` and it is no longer possible to continue the run.

  In other situations, a response might be generated without action by the client, for example, if a `input!` operation times out, the code that follows the operation will be executed without action by the client. If the client interacts with the server using a REST API, this means the response will be generated on the server side, but will not be displayed by the client. In this situation, the code should arrange to notify the client that a new response is available. In the following, the default value is used as a signal to trigger code which contacts the user and requests they return to the client application:

```clojure
(deflow bug-user-for-input []
  (output! "Please answer within a day"))
  (let [input (input! :expires (-> 1 day from-now) :default :timed-out)]
     (when :timed-out
        (notify-user-by-email
          :subject "Waiting for your response"
          :body "We're still waiting for your reply at https://app.com/chatbot/?run-id=f3162328-97b9-4f67-a15a-c77f1d9647d5")
        (recur))) ;; recursively retry
  (output! "Thanks for replying!")
  input) ;; return the user's input
 ```

  Note what happens here: the run times out, sends an email then executes the top of the flow again, generating the `"Please answer within a day"` response and suspending for input. However, no HTTP request exists to consume the response. For example, the user may have closed his browser. Nonetheless, the runtime still saves the run to durable storage together with the response generated thus far in this runlet. When the user reads the email and clicks the link, his browser loads a page on a web server. The web server uses the run id parameter to issue a `GET` request to retrieve the stored run from the rapids server and uses it to display the pre-generated response (`"Please answer within a day"`).

  A slightly different approach would be used for web socket-based systems. Timeout code would check for the presence of an active socket and send a response and only fail-over to email if the socket is no longer active.

### Summary

  The architecture includes the following elements:

  A. A compiler which partitions code bodies and inserts Stack Push operations in existing code. 

  B. Suspending operations which return Suspend signals at runtime

  C. partition functions stored at unique addresses

  D. partition functions which save new stack frames to the stack at runtime

  E. A stack composed of stack frames, each of which contains the address of a partition function, a map of lexical bindings, and an optional input-key

  F. A main loop function which continuously pops stack frames and evaluates partition functions until a `Suspend` signal is received.

  G. A means of saving the stack to persistent storage

  H. A means of retrieving the stack from persistent storage and resuming the loop

  I. A means for starting runs, redirecting clients to new runs, releasing control back to parent runs and blocking until other runs are complete

  J. A server which maintains flows and interacts with clients

## Definitions

   * partition - block of code from a (deflow ...) expression
   * address - a Clojure record which identifies a location in a flow
   * partition function - Clojure function associated with an address that implements the partition. In practice, this also includes code which pushes and pops frames onto/off of the stack
   * input - stop execution of a run due to encountering the input! command with a flow. The state of the flow is saved to the stack and can be resumed when an event is received.
   * suspending expression - within a lexical body of code, an expression which represents a point at which execution may pause for an arbitrarily long period of time.
   * stack frame - a Clojure record which contains information required for resuming a flow. It contains an address which identifies the next partition function, plus any bindings, and an optional input-key, representing a variable name to which a value will be bound when the computation is resumed in the next partition function.
   * stack - a FILO queue of stack frames
   * run - a data structure persisted in durable storage which is identified by a unique ID and stores the stack, but can also be used to refer to a sequence of execution involving multiple runlets and the events that trigger them.
   * runlet - the sequence of events following a start! or continue! call during which one or more partition functions are invoked. During a runlet, partition functions from multiple flows may be invoked. A runlet ends either when the run finishes (when the main flow completes and the stack is empty) or when a suspend signal is generated.
   * child run - a run started by another run. Moral equivalent of a subprocess.
   * event - a real world event which causes the run to resume executing after being suspended
   * redirect - relinquish control to a child run. Clients will receive responses from the child run and will continue the child run until it blocks or ends.
   * permit - value provided to input! which serves as a unique identifier for an event which may continue the run. 

#### Caching Runs in the run context

  Multiple runs may interact during a request. Runs may block and redirect to each other potentially several times during the servicing of a single request, so returning constantly to persistent storage would result in poor performance. A method for ensuring consistency while maintaining performance is to retrieve them all from storage using optimistic locking, and keep an index of these runs in the runtime cache. Since requests are expected to complete relatively quickly and since runs coordinate with real world entities or processes related to individuals, frequency of access is expected to be on very long time scales, so contention for runs is not expected.

### Run States

  * `:running` - Code is being executed by a CPU. The run is started in this state. The runstore does not reflect the current state of the run.
  * `:suspended` - Code is not running; the current state of the computation is fully captured in the RunStore. The run moves out of `:suspended` state when:
    
     * an external event is received (which matches the Suspend params)
     * a child run started with the blocking operator `<!` completes
     * a child run started by redirection `>>` blocks

  * `:complete` - evaluation complete, indicates result field was set
  * `:error` - the run is in an error state

### Run Return Modes

  * `:block` mode is initiated using the `block!` (aka  `<!`) operator. It takes a run as an argument and suspends until the run completes. Although you could achieve the same behavior by simply calling a flow directly from another flow, it is better to launch a separate run to handle inputs from a different type of process. But the most important aspect of `:block` mode is that it causes a run suspended by `:redirect` to resume, as described in the following.

  * `:redirect` mode is entered using the `>>` operator, which takes a run as an argument. We refer to it as the child run. The Suspend permit is the id of the child run, and when the child run enters a blocked state, it calls `continue!` with its own id as the permit and returns the parent run to the caller, thus resuming the parent run.

```clojure
(continue! parent-run-id :permit child-run-id :result child-run)
```

### Pools: Inter-run communication and synchronization

  The existing infrastructure provides the basis for a powerful mechanism of communication and coordination related to [channels](https://en.wikipedia.org/wiki/Channel_(programming)). We refer to them as "Pools" to distinguish them from [Clojure channels](https://clojure.org/news/2013/06/28/clojure-clore-async-channels ). 

  Pools provide a means for runs to communicate and synchronize with each other. Values are put into pools using the `put-in!` flow and taken out using the `take-out!` flow. Pools contain an internal buffer containing zero or more slots. When the buffer is full, the`put-in!` operator will suspend the current run until space becomes available. When no put-ins are pending on the pool and the buffer is empty, the `take-out!` operator suspends the current run until a value is available. This is similar behavior to `put` and `take` operators in other CSP-inspired features, such as in golang and in Clojure's async library.

  This section shows how a channel-style capability can be implemented using the existing infrastructure. 

  First an example.
 
#### Simple example suspending take-out using an unbuffered pool

```clojure
(deflow source-flow [p] 
  (let [user-input (<*)] ; get a value from the user
    (put-in! p user-input))) 
    
(deflow sink-flow []
  (let [p (pool)] ; creates an unbuffered pool
    (let [source-run (start! source-flow [p])]
      (<*  (take-out! p))))) ; take a value out of a pool and use it as a response

(def sink-run (start! sink-flow))
(continue! runA :foo)

```
  This code causes `take-out!` to suspend while it waits for the source to produce a value. This is the sequence of events:
  
  1. `sink-run` is started using `sink-flow` (line 10)
  2. `sink-flow` creates an unbuffered pool, `p` (line 6)
  3. `sink-flow` creates `source-run` starting it with `source-flow` and passing `p` (line 7)
  4. `source-flow` suspends, waiting for user input (line 2)
  5. `sink-flow` attempts to take out from `p` and suspends, since no value is available in `p` (line 8) 
  6. `sink-run` continues with value `:foo`  (line 11)
  7. `sink-run` puts `:foo` in the pool (line 3)
  8. the `take-out!` call (line 8) resumes, returning value `:foo`  

  Alternatively, if we were to assign `:foo` to `user-input` directly (at line 2) - 

```clojure
  (let [user-input :foo] 
```
  - then, `source-flow` would suspend on the `put-in!` call because in this situation the unbuffered pool would not yet have any pending take-outs. Notice that unbuffered pools provide a facility similar to semaphores. Unbuffered pools allow tight coupling of two runs at `put-in!` and `take-out!` calls. 

#### Buffered pools

  Buffered pools permit decoupled `put-in!` and `take-out!` calls. A buffered pool is created with a fixed number of slots by passing a positive integer to `pool`:

```clojure
(pool 3) ; creates a pool with 3 slots
```
#### Implementation of Pools

  The `pool` function initializes and returns an instance of the `Pool` data structure: `(pool n)`. It takes a single optional argument, the buffer size, which defaults to 0. 
  
  In Clojure, the data structure would be represented as follows:     

```clojure
(defrecord Pool
   id     ; a globally unique identifier  
   size   ; integer, indicating size of the buffer
   sources  ; FIFO queue holding run-ids representing runs waiting to put-in a value
   buffer   ; FIFO queue for buffered values
   sinks)   ; FIFO queue of run-ids representing runs waiting to take-out a value
```
  The `sources` queue represents blocked `put-in!` calls. It stores "put-in records", two tuples which record a run-id and a value, and which can be represented by a Clojure record:

```clojure
(defrecord PutIn run-id value)
```
  We'll write `PutIn(R, V)` to represent PutIn records below, where R is a run id and V is a value.

  The `buffer` queue stores *values* which are available for `take-out!`. A pool's buffer is *full* when the `buffer` queue contains `size` items. It may not hold more than `size` items. A pool with a zero buffer size is referred to as an unbuffered pool, and is always considered full.   

  The `sinks` queue represents runs which have been suspended by `take-out!`. In practice, it is a FIFO queue of run-ids.

  Note that the `buffer` and `sources` queues are filled by `put-in!` and emptied by `take-out!`. Conversely, the `sinks` queue is filled by `take-out!` and emptied by `put-in!`. If the `sources` or `buffer` queues are not empty, the `sinks` queue must be empty, and conversely, if the `sinks` queue is not empty, then both the `sources` and `buffer` queues must be empty. The rules implemented by `put-in!` and `take-out!` are outlined in the following section.

  In the following, we refer to the run id of the current run as `*current-run-id*` for convenience:

#### The put-in! Algorithm

  Given a call to `(put-in! pool new-value)`, we update queues in `pool` as follows: 

    - ADD new-value to BUFFER queue
    - IF the sinks queue is not empty // if so, buffer and sources queues are empty
      THEN
        - REMOVE a run-id from the SINKS queue
        - REMOVE a value, buff-val from the BUFFER queue
        - CONTINUE sink with buff-val using (continue! run-id buff-val)
        - RETURN nil
      ELSE
        - IF the BUFFER queue is full
          THEN
            - ADD the current run-id to the SOURCES queue
            - SUSPEND the current run using (input! :permit pool-id)

  This algorithm assumes the pool object is destructively modified. In Clojure this requires passing the pool inside an atom.  

#### The take-out! Algorithm

  Given a call `(take-out! pool)`, we update queues in `pool` as follows: 
  
    - IF BUFFER queue is empty
        THEN
          - ADD current run-id to SINKS queue
          - SUSPEND current run
        ELSE
          - REMOVE a value from BUFFER, storing as result
    - IF SOURCES queue is not empty
      THEN
        - REMOVE run-id source from SOURCES queue
        - CONTINUE source
    - RETURN result
        
    
  This algorithm assumes the pool object is destructively modified. In Clojure this requires passing the pool inside an atom.

#### Securing Pool flows with a permit

  The `put-in!` and `take-out!` algorithms should guard against invalid continuation of runs. To do this, the above algorithms should be modified to include a `:permit` argument. A simple choice wis to use the pool id. So instead of calling `(input!)` and `(continue! run-id value)`, we would call `(input! :permit (:id pool))` and `(continue! run-id value :permit (:id pool))`.   

#### Storing and regenerating pools

  When a runlet ends, the system arranges for any pool objects bound to local variables to be persisted to durable storage as described above using custom `freeze` and `thaw` methods. Pools are persisted as storage-based objects, as described above under ["Storing and Regenerating State"](#storing-and-regenerating-state). This means only the id of the pool is stored in the stack. The pool object is retrieved from durable storage and the record is locked for the duration of the runlet. An alternative approach is to retrieve and lock pool data from durable storage only when data is accessed.

  Note that that although several runs may be active during a runlet, the code in those runs does not executes sequentially. For example, a run may `start!` another run, passing it a pool, and the second run may suspend after attempting to `take-out!` from the pool, thereby return control to the first run. The mechanism described here avoids unnecessary reads and writes to durable storage, permitting efficient communication between multiple runs executing during a runlet. 
 
## REST API Implementation

  An obvious use case is to provide the rapids service over a web endpoint. The implementation is simply involves mapping web endpoints to the high level functions described above. It's a trivial exercise to imagine an implementation for websockets, and other transport protocols.

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

  Given the chatty nature of the applications discussed here, a websockets implementation also seems like a natural choice.  The current approach could be trivially adapted to enable it. Alternatively, it may be useful for flows to write directly to the websocket. The client code could arrange for the socket to be passed as an argument to flows, for example.