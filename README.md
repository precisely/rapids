# longterm

A DSL for programming long running flows, involving interactions with the real world which may occur over minutes, days, or months. This library is intended to make it easy to write sophisticated user flows. 

Longterm defines a new macro, `deflow`, akin to `defn`, but which  permits listening execution until an external event is received. This is done with the `(listen! :permit :permit context)` special form. The system uses a user-definable RunStore which stores the state of the computation when a `listen!` is encountered. A default in memory runstore is provided, but the system is intended to be used with persistent storage. 

Execution is restarted using `(continue! run-id context optional-result)`. The result provided to `continue!` becomes the value of the `listen!` expression in the ensuing computation, which continues until complete or another `listen!` is encountered.  

## Basic Usage
Also see `tests/longterm_test.clj`.

### Define a flow
```clojure
(deflow multiply-by-user-input [x]
  (respond! {:type :chat
             :text "Hi, please enter a number!"})
  (respond! {:type :number-input          ;\__ interpreted by caller to display
             :type :number            ;/   user input UI
             :context :user-number}) ; caller uses this when sending event
  (* (listen :user-number) x))       ; returns the value the user entered multiplied by x
```

### Start the flow
```clojure
;; a call to an API initiates a run, requesting the flow to start
;; the following code starts the flow and returns a response to the caller

(let [run (start! multiply-by-user-input 4)]
  ;; somehow return the result to the caller 
  (return-result-to-caller {:run-id (:id run) :response (:response run)})
  ...)
```

### Resume the flow 
```clojure
;; the caller provides run-id, context and data 
;; resume the flow as follows:
(let [run (continue! run-id context data)] ; data = the number
   (return-result-to-caller {:run-id (:id run) :response (:response run)}))
```

## Implementing a custom storage backend

### Import Run and IRunStore from longterm.runstore 

```clojure
(ns my.package
  (:require [longterm.runstore :as rs]))

...

(extend MyDBAdapter
  rs/IRunStore ; 
  (rs/rs-create! [rs state] ...) ; return an object implementing rs/IRun in the given state in the db 
  (rs/rs-get [rs run-id] ...) ; find and return the run
  (rs/rs-acquire! [rs run-id] ...) ; atomically changing existing run from :listening to :running state and return it
  (rs/rs-update! [rs run] ...) ; save the given run to the db
```
The methods of IRunStore should return Run instances. If additional fields are needed, simply `assoc` them onto this object. 




## Deployment

This library uses the [s3-wagon-private plugin](https://github.com/s3-wagon-private/s3-wagon-private) to deploy to and consume the artifact from a dev-precisely S3 bucket (precisely-maven-repo as time of writing). 

To push a release:

1. Commit the code
2. Update the version in project.clj and tag the release

   If you use `"0.2.0-SNAPSHOT"` in project.clj, do `git tag 0.2.0-SNAPSHOT` 
   
3. Push to github

   ```git push --tags```
   
4. Ensure AWS access is configured

   You need to provide access key id & secret to push to the S3 bucket. You will need write access, obviously. These are stored in .env. The .env.sample file contains the key names: 
   
   ```
   cp .env.sample .env
   ```
   
5. Deploy the library to S3
   ```shell script
    lein deploy precisely
    ```