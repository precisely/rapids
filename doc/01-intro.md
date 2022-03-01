# Introduction to Rapids

Rapids lets you build workflows using functional programming techniques. Unlike a function, a workflow needs to pause for arbitrarily long periods while it  waits for a human (or robot) to complete a task and possibly enter some data. In Rapids, workflows ("Flows") are defined using the `deflow` macro, which is analogous to Clojure's `defn`. The arguments are the same:

```clojure
(defn [name docstring? & sigs] ...)
(deflow [name docstring? & sigs] ...)
```
The code bodies inside `deflow` can include most Clojure expressions, such as function calls, literals of all kinds, special operators, macros and Java interop. `deflow` bodies can also use some special operators which allow programs to work over arbitrarily long time scales. Flows are typically used in the context of web interactions to build scalable web services that need to manage complex state. Flows are defined much like functions:

```clojure
(defn multiply [x y]
  (* x y))
  
(deflow multiply [x y]
  (* x y))
```

The `deflow` form adds a few key operators which allow the programmer to wait for input from an external entity. 

Use the `input!` operator, usually written `<*`, to pause the workflow for input. 


 I.e., it takes inputs and produces an output. Along the way several interactions with one or more people may be necessary, but from our perspective as Rapids programmers, this is secondary.  We call these workflow functions "Flows", and refer to this approach as "functional Human Computer Interaction programming".

Flows are defined using the `deflow` macro, which has a similar signature and behavior to Clojure's `defn`:

```clojure
(deflow chatbot []
  )
```


 