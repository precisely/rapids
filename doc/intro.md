# Introduction to Rapids

Rapids implements a technique that treats real world processes as functions. For example, a multi-step process involving several human-computer interactions that aims to check a users cholesterol is modeled as something akin to a function. I.e., it takes inputs and produces an output. Along the way several interactions with one or more people may be necessary, but from our perspective as Rapids programmers, this is secondary.  We call these workflow functions "Flows", and refer to this approach as "functional Human Computer Interaction programming".

Flows are defined using the `deflow` macro, which has a similar signature and behavior to Clojure's `defn`:

```clojure
(deflow 
```


 