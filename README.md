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
  (* (listen!) x))       ; returns the value the user entered multiplied by x
```

#### respond! (shorthand: *>)

```
(respond! arg*) ; or (*> arg*)
```
Write an object to the `:response` key.

The `respond!` operator is conceptually akin to writing to stdout, but the output is collected and returned as the `:response` key of a run object returned by `start!` or `continue!` when a flow hits a `listen!` or completes execution.

`respond!` take an arbitrary number of objects which are appended to the current response vector. Note that the response vector is automatically cleared before a run is continued so each request only retrieves some of the `respond!` arguments in a flow.

#### listen! (shorthand: <*)

Suspends execution of the run until a call to `continue!`.
 
```
(listen!) ; or (<*)
(listen! :permit permit)
(listen! :expires expiry-time, :default value)
(listen! :permit permit, :expires expiry-time, :default value)
```

A call to `listen!` causes the run to be persisted to storage. Execution is resumed by calling `continue!` and providing the run-id, the permit value (which is nil by default) and a `result` value. When the run resumes, the `(listen!...)` form evaluates to the `result` value. 

When the expiry time is passed, execution resumes, with the `listen!` operator evaluates to the value of the `default` argument, which is nil if not provided.

#### block! (shorthand: <<!)

Suspends execution of the current run until the given run completes. Returns the value returned by the given run. If the current run was redirected, control passes back to its parent.

```
(block! run) ; or (<<! run)
(block! run :expires expiry-time, :default value) 
```

#### redirect! (shorthand: >>)

Suspends the current run and passes control to the given run.

```
(redirect! run) ; or (>> run)
```

Adds the passed run's response to the current response, and sets the run as the next run. The current run is suspended until the passed run completes or blocks. The value returned will be the passed run in a suspended  or completed state.

### Starting a flow
```clojure
;; a call to an API initiates a run, requesting the flow to start
;; the following code starts the flow and returns a response to the caller

(start! multiply-by-user-input 4)
```

### Resume the flow 
```clojure
;; the caller provides run-id, context and data 
;; resume the flow as follows:
(continue! run-id permit data)
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

### S3 Deployment
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
   
### Local Deployment

During development, you may want to publish to a local repo instead of to S3.  This can be done by publishing to your local Maven repo. This is typically at `~/.m2`.  The dev profile has the [lein-localrepo plugin](https://github.com/kumarshantanu/lein-localrepo) installed.

First build your target:

```
lein build
``` 