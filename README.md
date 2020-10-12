# longterm

A DSL for programming long running flows, typically involving interactions with the real world which may occur over days, weeks or months. This library is intended to make it easy to write sophisticated user flows. 

Longterm defines a new macro, `deflow`, akin to `defn`, but which  permits suspending execution until an external event is received. This is done with the `(suspend! event-id)` special form. The system uses a user-definable RunStore which stores the state of the computation when a `suspend!` is encountered. A default in memory runstore is provided, but the system is intended to be used with persistent storage. 

Execution is restarted using `(process-event! run-id event-id optional-result)`. The result provided to `process-event!` becomes the value of the `suspend!` expression in the ensuing computation, which continues until complete or another `suspend!` is encountered.  

## Basic Usage

### Define a flow
```clojure
(deflow multiply-by-user-input [x]
  (respond! {:type :chat
             :text "Hi, please enter a number!"})
  (respond! {:type :number-input          ;\__ interpreted by caller to display
             :type :number            ;/   user input UI
             :event-id :user-number}) ; caller uses this when sending event
  (* (suspend :user-number) x))       ; returns the value the user entered multiplied by x
```

### Start the flow
```clojure
;; a call to an API initiates a run, requesting the flow to start
;; the following code starts the flow and returns a response to the caller

(let [run (start-flow! multiply-by-user-input 4)]
  ;; somehow return the result to the caller 
  (return-result-to-caller {:run-id (:id run) :response (:response run)})
  ...)
```

### Resume the flow 
```clojure
;; the caller provides run-id, event-id and data 
;; resume the flow as follows:
(let [run (process-event! run-id event-id data)] ; data = the number
   (return-result-to-caller {:run-id (:id run) :response (:response run)}))
```

Also see `tests/longterm_test.clj`.

