# Introduction to Rapids

Rapids lets you build workflows using functional programming techniques. Unlike a function, a workflow needs to pause for arbitrarily long periods while it  waits for a human (or robot) to complete a task and possibly enter some data. In Rapids, workflows ("Flows") are defined using the `deflow` macro, which is analogous to Clojure's `defn`. The arguments are the same:

```clojure
(defn [name docstring? & sigs] ...)
(deflow [name docstring? & sigs] ...)
```
The code bodies inside `deflow` can include most Clojure expressions, such as function calls, literals of all kinds, special operators, macros and Java interop. `deflow` bodies can also use some special operators which allow programs to work over arbitrarily long time scales. Suspending for input, handling interruptions while suspended, and sending data to the outside world as  

```clojure
(defn multiply [x y]
  (* x y))
  
(deflow multiply [x y]
  (* x y))
```

So far so good. But running flows 


What `deflow` adds are a few key operators which permit the workflow code to operate over very long time scales without consuming volatile memory or compute resources while waiting for input. 

Use the `listen!` operator, usually written `<*`, to pause the workflow for input. Think of the asterisk as a globe representing the real world. This operator says "get data from the real world".


 I.e., it takes inputs and produces an output. Along the way several interactions with one or more people may be necessary, but from our perspective as Rapids programmers, this is secondary.  We call these workflow functions "Flows", and refer to this approach as "functional Human Computer Interaction programming".

Flows are defined using the `deflow` macro, which has a similar signature and behavior to Clojure's `defn`:

```clojure
(deflow chatbot []
  )
```


 