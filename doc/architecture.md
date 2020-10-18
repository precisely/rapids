# Architecture

## Intro
The `deflow` macro enables writing procedures, called flows, which await events in the real world, and which may occur over long periods of time. They’re conceptually similar to asynchronous processes in Javascript and other languages, however, while async functions and threads execute within the context of a single continuously running computer, the execution of a longterm flow may pass between different CPUs, as the program pauses for external events.

The macro analyzes a body of code, determining places where execution pauses, and breaks each segment into separate functions which can pause and resume, with the bindings from the lexical environment regenerated. For example, consider this fragment:

```clojure
(deflow foo [a b]
  (let [result1 (flow1 a)
        result2 (flow1 b)]
    (print (flow2 result1 result2)))))
```

The `foo` flow calls two subflows and passes the results to a third flow, which returns a result to the clojure print function. While this looks like nothing more than programming with ordinary functions, imagine that `flow1`, `flow2` and `flow3` each involve gathering input from users, perhaps involving contacting them by email, waiting for them to login and provide some input, etc. 

Since these activities span a long time, it isn't feasible to dedicate an active process to it. Therefore, the program must be stopped at various points and relevant data about the execution context must be persisted so that at a later stage, when an event is received, the computation can resume. 

### Definitions
   * partition - block of code from a (deflow ...) expression
   * address - a Clojure record which identifies a location in a flow
   * continuation - Clojure function associated with an address that implements the partition. In practice, this also includes code which pushes and pops frames onto/off of the stack
   * listen - stop execution of a run due to encountering the listen! command with a flow. The state of the flow is saved to the stack and can be resumed when an event is received.
   * stack frame - a Clojure record which contains information required for resuming a flow it contains an address which identifies the next continuation, plus any bindings
   * stack - a FIFO stack of thunks; the most recent represents the next continuation
   * run - a sequence of execution involving multiple runlets and the events that trigger them.        also the object stored in the database that tracks the run
   * runlet - a sequence of continuations which are invoked while handling an event, representing partitions from potentially many flows. A runlet begins when a flow is started or continued and ends when a listen! operation is encountered. 
   * child run - a run started by another run. Moral equivalent of a subprocess.
   * event - a real world event which causes the run to resume executing after being listening
   * cede - relinquish control to a child run 
   * context - value provided to listen! which identifies the type of event which may continue the run. Sometimes referred to as the "listen context". Runs may cede control to child runs until the child run leaves a particular context. For example, a run responding to a user may cede to a child run to initiate an activity with them. At some point, the child run switches context from gathering input from the user to listening for some other real world event, like results from a lab. When the child flow's listen context

 ### Mechanism
 1) During the partitioning phase, inside the deflow macro, code is analyzed and flow expressions are identified - expressions like `(myflow ...)`, where `myflow` is a global var bound to a flow.
    
 2) Code is split at each flow expression. Sometimes a flow will be deeply embedded inside the AST possibly inside branching logic. 
 
 3) Flow expressions which appear in a non-terminal position are wrapped in code which pushes a Thunk onto a stack pointing at the next location.  For example, when a flow  in `(do (a) (b) (myflow))`, `(myflow)` is terminal - there are no forms after `(myflow)`, so a Thunk is not required. But in `(do (a) (myflow) (b))`, a thunk is required to allow execution to resume after `(myflow)` completes at `(b)`. This is done with: `(resume-at [{some-address}] (do (a) (myflow)))` which causes a Thunk record is pushed onto the stack.  
 
    Thunks also tell where to bind the result of an expression. For example in:
    
    ```
    (let [x (flow1)] 
      (dosomething x))
    ```
    
    We get:
    ```(resume-at [:p2 'x] (flow1))```
    Which means "after flow1 completes, continue execution at address :p2 and bind the result to `x`". Note that addresses are not keywords, I'm just using keywords here for simplicity. 
       
    Note, however, that the `resume-at` wrapper is applied whenever an expression contains a flow expression and computation must resume in another continuation. It does not necessarily wrap a flow form directly. 
    
    At some point, the containing expression may be non-terminal, in which case the entire expression, which contains the flow call nested inside, is wrapped with `(resume-at ...)`. This mechanism ensures that whether a conditional branching structure stops at a flow or a normal value, the value will be correctly passed to the next partition, either immediately in the case of a non-flow expression or later if execution has stopped at a flow. 
    
    For example, consider a flow `user-survey` which implements logic like:
     
       ```clojure
       (deflow user-survey [s] 
          (or (get-survey-results-from-db s)    ; step 1 completes immediately
              (launch-survey-dialog s)))        ; step 2 only runs if no results in db
       ```    
    
    Note that the `user-survey` flow may return a value immediately. You'll see below how the evaluation mechanism handles these cases.
     
 4) Runs are started using the `start-run` macro: `(start-run form)`, where form is an expression which calls a flow. It creates a new `run-id` which is analogous to an operating system's process-id. It is used to save and retrieve the stack and associated events for a particular flow.
 
 
 5) Under the hood, `start-run` establishes a new stack with a single Thunk and that calls `form`, and then calls `next-runlet.
 
 6) The `next-runlet` function pops the thunk at the top of the stack and calls the continuation.
 
 7) When the runlet finishes, `next-runlet` examines the return value. If it is the special +DELAY+ value, no further action is taken, otherwise,`next-runlet` recursively calls itself, providing the return value as an argument. The stack is popped, and the return value is bound to the result-key in the Thunk (if provided), and the Thunk continuation is invoked.
 
 8) If the stack is empty, `next-runlet` stores the return value in the RunStore (an interface to a persistent storage) associated with the `run-id`. The run is now complete.

 In the following examples, I use `fl{n}---` to represent flows (the dashes are just ot help  visually identify the flows in the code) and letters at the beginning of the alphabet to represent ordinary functions, and letters from the end for variables
 
 First a simple example:
```clojure

(deflow fl1--- [z]
  (fl3---
    (a)                                 ; \___ this code runs first (:p1 = "partition 1")
    (fl2--- z)                          ; /
    (b)))

(deflow fl2--- [name]
  (say "Hello, " name)
  (say "How are you doing?")
  (wait-for :user-input))

{
 :p1 (fn [{:keys [z]}]
       ;; resume-at pushes a StackFrame onto the stack - it contains the following info:
       (resume-at [:p2                  ; partition to resume at
                   'arg1                ; where the result should be stored
                   {z    z              ; a map of bindings
                    arg0 (a)}]
         (start 'fl2 z)))               ; calls the entry-point of fl2

 :p2 (fn [{:keys [z arg0 arg1]}]
       ;; because the fl1 call is the last in deflow,
       (start 'fl1 arg0 arg1 (b)))      ; calls the entry-point of fl1
 }
```

Now a more complicated flow which exercises all of the basic cases i can think of:
   * let binding a flow or a complex expression which contains a flow
   * branching logic which sometimes results in normal values, sometimes in a delay
   * flows which end a sequence
   * flows in the middle of a sequence
   * flows as function arguments
   
```clojure
(deflow fl0--- [z]
  (let [x (do (m)
              (if (a)
                (if (b)
                  (fl1---)              ; do not push thunk here because flow ends here
                  (c))
                (if (d)
                  (if (fl2---)          ; push thunk because flow continues to if form (at :p3)
                    (let [y (fl3---)]   ; push thunk because flow continues to let body (at :p4)
                      (e y))
                    (do
                      ;; ignore return value of (fl3---)
                      (fl3---)          ; push thunk because flow continues to (f) (at :p6)
                      (f)))

                  ;; Flow as function argument
                  (fl4---               ; this runs after args are evaled
                    (fl5---)            ; push thunk to continue flow at fl4--- call
                    (g)))))]
    (f x)))

;; Produces the following ocntinuation set:
{
 :p1 (fn [{:keys [z]}]
       (resume-at [:p2 x]               ; push-thunk :p2 x
         (do (m)
             (if (a)
               (if (b)
                 (fl1---)               ; because flow stops here, don't push thunk
                 (c))
               (if (d)
                 (resume-at [:p3 if-arg0 {z z x x}] (fl2---)) ; push-thunk :p3 if-arg0
                 (do
                   (resume-at [:p7 fl4-arg0 {z z x x}] (fl5---)))))))) ; push-thunk :p6

 :p2 (fn [{:keys [z x]}] (f x))

 :p3 (fn [{:keys [z x if-arg0]}]                ; pop-thunk (:p3 is on top), revealing :p2
       (if if-arg0
         (resume-at [:p4 y {z z x x}] (fl3---))   ; push-thunk :p4
         (do (resume-at [:p6] (fl3---))))) ; push-thunk :p6


 :p4 (fn [{:keys [z x y]}]
       (e y))                           ; pop-thunk :p4 (on entry)
 ; pop-thunk :p2 (because (e y) returns non-delay)

 :p5 (fn [{:keys [z x]}]
       (c))

 :p6 (fn [{:keys [z x]}]
       (f))

 :p7 (fn [{:keys [z x fl4-arg0]}]
       (fl4--- fl4-arg0 (g)))
 }
```