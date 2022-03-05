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

### Defering actions for later

### Interrupting runs

