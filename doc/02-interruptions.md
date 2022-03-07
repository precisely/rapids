# Handling interruptions

During execution of a flow, it may be necessary to halt execution or redirect the flow. For example, a healthcare worker might determine that a patient should be taken off of an automated drug dosing management program. Alternatively some parameters might need to be changed, and a process restarted - for example, a dosage might need to be manually increased.

In Rapids, you can handle situations like these using `attempt` expressions. They are analogous to `try` expressions, in that normal control flow is redirected.  Unlike `try` expressions, which handle exceptions generated at runtime, `attempt` handles _interruptions_, which are generated external to a flow (for example, by a system administrator managing automated patient flows). Furthermore, although exceptions are generated during runtime, interruptions occur while a flow is suspended. 

## attempt 
The `attempt` macro signature is as follows:

For example,
```clojure
(deflow my-interruptible-flow []
  (attempt
    (foo)   ; a flow during which interrupts may happen
    (restartable (bar)   ; an expression which may be restarted
      (:redo [o] (* o o)) ; restart can take any number of arguments and returns a value based on the arguments
    (handle :xyzzy i
      (println (:input i)) ; prints interruption's data to stdout
      ...)                ; handle :xyzzy block's value is returned when an :xyzzy interrupt is encountered
    (handle :recalc i
      (restart :redo (:data i))  ; :recalc interrupts are handled by passing the data to the :redo restart 
    (finally (do-stuff1) (do-stuff2)) ; this code is guaranteed to execute after either the attempt,
                                      ; but the return value is ignored
```

The `interrupt!` top-level API function can be used to generate an interruption when the run is suspended. This method visits attempt blocks checking for a matching interruption handler (the second argument should match the name field of the interruption). Note that while `attempt` expressions are used inside `deflow` bodies, `interrupt!` is a top-level function used for controlling runs, usually used outside of `deflow` (for example to implement a server API). It is a sibling of top-level functions, `start!` and `continue!`.

```clojure
(interrupt! run-id :xyzzy :data {:a 1 :b 2})
```

### handle

Handlers are conceptually similar to `catch` clauses in expressions like `(handle :iname ivar ...body)`. The first argument, `iname`, is a keyword. When the handlers `iname` matches the name field of an interruption, the interruption is bound to ivar and the body is executed. The value generated by the last expression in body is returned by the attempt block.

### finally

Finally blocks behave analogously to `finally` in `try` expressions. They take a list of expressions as an argument. The attempt macro guarantees that this code is executed immediately after the attempt body or a handler body is executed.

### restartable and restart

Inside attempt bodies, expressions can be wrapped with `restartable`, as `(bar)` is above. Restartable expressions take an expression as a first argument and one or more restart definitions as remaining arguments. Restarts are essentially single arity function definitions with a keyword the name. A restart can take zero or more arguments. 

Restarts are invoked in handlers using the `restart` flow. 

## Interruptions

Interruptions are objects which implement the IInterruption interface (defined in rapids.objects.interruptions). For the most part, you'll just use the built in `Interruption` record type for all your needs. An interruption record is generated implicitly when you pass a keyword as a second argument to `interrupt!`:

### Future work
Not yet implemented are functions for enumerating restarts, and the ability to restart at points lower than the current attempt. This could be achieved by adding restarts to the interrupt as it passes up the chain. 