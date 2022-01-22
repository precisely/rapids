## Detailed discussion 
 
 1) During the partitioning (compiler) phase, the abstract syntax tree is analyzed by recursively walking the tree. The compiler examines each expression and determines whether it is a suspending expression. Suspending expressions include invocations of the `input!` function, invocations of flows or expressions which contain nested suspending expressions.
 
 2) The compiler generates a symbolic address as it visits each expression. For example, in the following `do` block, the addresses. Each continuation is associated with the address which begins the continuation. For example, the address of the first continuation in the main flow is `main/0`.
 
 ```clojure
 (deflow greeting [day-of-week]
   (output! "Hi. What is your name?")  ; address = greeting/0
   (let                                 ; address = greeting/1 
      [name (input!)]                  ; address = greeting/let/0
     (output! "Nice to meet you" name) ; address = greeting/let/1/0
     (output! "It's a very nice " day-of-week) ; address = greeting/let/1/1
     name)) ;; return the name of the user
```
    
3) Each code body is recursively analyzed for suspending expressions. If a suspending expression is found in a non-terminal position, the body is partitioned at that point. Code is injected before the suspending expression which ensures a stack frame will be pushed onto the stack at runtime. The stack frame will contain the address of the next partition. In this way, suspending expressions are always positioned as terminal expressions of continuations.
 
  E.g., in 
  ```clojure
   (do (a) (b) (myflow))
  ``` 
 
  The `(myflow)` call is a suspending expression, but is terminal. The compiler defines a single partition, coroutputing to a single continuation. But in `(do (a) (myflow) (b))`, execution may suspend at `(myflow)`, so the partition must end there, and a stack frame must be pushed onto the stack to allow execution to resume at `(b)`. This is done with a macro: 
  ```clojure
    (resume-at [{address-of-b}] (do (a) (myflow)))
  ```
 
  Stack frames also tell where to bind the result of an expression. For example to pass the result of `(flow1)` to `(dosomething)`, we could bind it to a variable, `x`:
        
  ```clojure
   (let [x (flow1)] 
     (dosomething x))
  ```

  The compiler produces two continuations, one containing code like:
   1. continuation 1: ```(fn [& {:keys []}] (push-stack-frame '[continuation-2-address {} 'x]) (flow1)))```
   2. continuation 2: ```(fn [& {:keys [x]}] (dosomething x))```
   
  Note that a suspending expression may contain a suspending operation nested deep inside. This mechanism ensures that whether a conditional branching structure stops at a flow or a normal value, the value will be correctly passed to the next partition, either immediately in the case of a non-flow expression or later if execution has stopped at a flow. 

For example, consider a flow `user-survey` which implements logic like:
 
   ```clojure
   (deflow user-survey [s] 
      (or (get-survey-results-from-db s)    ; step 1 completes immediately
          (launch-survey-dialog s)))        ; step 2 only runs if no results in db
   ```    

Note that the `user-survey` flow may return a value immediately. You'll see below how the evaluation mechanism handles these cases.
     
 4) Runs are started using the `start!` function: `(start! flow ... args)`, where flow evaluates to a flow. It creates a new `run-id` which is analogous to an operating system's process-id. It is used to save and retrieve the stack and associated events for a particular flow.
 
 
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