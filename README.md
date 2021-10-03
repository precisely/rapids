# Rapids
![dev](https://github.com/precisely/rapids/actions/workflows/coverage/badge.svg?branch=dev)
![master](https://github.com/precisely/rapids/actions/workflows/coverage/badge.svg?branch=master)

A DSL for programming user interaction flows. 

Rapids defines a new macro, `deflow`, akin to `defn`, but which  permits suspending execution until an external event is received. This is done with the `(listen!)` special form. The system uses a persistent Storage which saves the state of the computation when the `listen!` operator is invoked. An in-memory and Postgres implementation are provided. 

The control API consists of two main functions, `start!` and `continue!` for starting and continuing flows. 

## Basic Usage
Also see `tests/Rapids_test.clj`.

### Define a flow
```clojure
(deflow multiply-by-user-input [x]
  (respond! "Hi, please enter a number!")
  (let [user-num, (Integer/parseInt (listen!))
        result (* user-num x)]
  (*> (str "Multiplying " x " by " user-num " gives " result))  
```
As of 0.3.2, `deflow` supports multi-arity signatures and pre/post conditions like `defn`.

### Simple example
```clojure
(let [run (start! multiply-by-user-input 5)]
   (continue! (:id run) {:data "100"}) ; this would normally happen as the result of a separate web API call
   (println (:result run))) ; prints 500
=> 
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

A call to `listen!` causes the run to be persisted to storage. Execution is resumed by calling `continue!` and providing the run-id (available using `(:id run)`, the permit value (which is nil by default) and `data` value. When the run resumes, the `(listen!...)` form evaluates to the `result` value. 

When the expiry time is passed, execution resumes, with the `listen!` operator evaluates to the value of the `default` argument, which is nil if not provided.

#### block! (shorthand: <<!)

Suspends execution of the current run until the given run completes. Returns the value returned by the given run. 

```
(block! run) ; or (<<! run)
(block! run :expires expiry-time, :default value) 
```

### Starting a flow

```clojure
;; start! creates a run, beginning execution of the given flow

(start! multiply-by-user-input 4)
  ...
```

### Resume the flow 
```clojure
;; the caller provides run-id, context and data 
;; resume the flow as follows:
(continue! run-id {:permit permit :data data})
```

## Setting up a backend

Rapids works by persisting state of flows in non-volatile storage. This capability can be provided by implementing the protocols, in rapids.storage.protocol: Storage and StorageConnection. The library contains implementations of an in memory implementation (used for testing) and a Postgres-based implementation.


### Setup a PostgresStorage Backend
```clojure
(ns mynamespace
  (:require [rapids :refer :all]
            [rapids.implementations.postgres-storage :refer [->postgres-storage postgres-storage-migrate!]])
(set-storage! (->postgres-storage {:jdbcUrl "jdbc:postgresql://{host}:{port}/{dbname}`}))
(postgres-storage-migrate!) ; uses the top-level storage by default          
```

## Testing

### IntelliJ / Cursive

The IntelliJ project has shortcuts for running tests under the Tools Menu. First, start the Clojure nREPL, then choose one of the following:

Tools -> Run Tests in Current Namespace in REPL
Tools -> Run Tests Under Caret in REPL
Tools -> Commands -> Run All Tests!

### Postgres tests

The test suite includes some tests for the Postgres backend which only run conditionally. To run them:

1. install postgres
   with homebrew: `brew install postgresql`
2. start postgres
   with homebrew: `brew services start postgresql`
3. create a test database: `createdb rapids-test`
4. include the following line in your .env file:
   `TEST_POSTGRES_URL=postgresql://localhost:5432/rapids-test`

### Command line

I've had some issues with running tests from the command line:
```shell
lein test
```

The `rapids.test` namespace includes a couple of `clojure.test` compatible macros (`branch` and `keys-match`) which make it easier to test branching flows. These are useful because the `start!` and `continue!` methods cause side effects on the run. 

Here's an example of how to use them:

```clojure
(deftest WelcomeTest
  (branch "welcome" [run (start! welcome)]
    (keys-match run
      :state :suspended
      :response ["welcome. Do You want to continue?" _])

    (branch "wants to continue"
      [run (continue! (:next-id run) {:data "yes"})]
      (keys-match run
        :state :suspended 
        :response ["great!... let's continue"]))

    (branch "doesn't want to continue"
      [run (continue! (:next-id run) {:data "no"})]
      (keys-match run
        :state :complete))))
```

### branch

Creates nested test conditions.

```clojure
(branch "description" [...bindings] & body)
```

### keys-match

A wrapper around `is` and `match` to make it easy to match patterns in maps:

```clojure
(keys-match obj-to-match :key1 pattern1 :key2 pattern2 ...) 
```

### Exception Types

Besides the usual Clojure program errors, this package throws `ExceptionInfo` objects with data containing a :type key, indicating the following 

* `:runtime-error` - an error caught at runtime, usually indicating a programmer error. E.g., passing the wrong type of argument to a function.
* `:system-error` - a severe error usually indicating a bug in the system or inconsistency of the stack
* `:syntax-error` - problem while compiling a flow
* `:input-error` - invalid data was provided to the system

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

### Notes

#### REPL vs CI Test inconsistency

Occassionally, tests will pass in the REPL, but fail in CI, or at the command-line with `lein test`. In the instances I've seen so far, this occurs because of differences in the macroexpansion environment between `lein test` and REPL environments inside test functions. I think this is because test functions are compiled by `lein test` before being executed, whereas they are evaluated by the REPL. This leads to symbols remaining unqualified at execution time in the `lein test` environment. 

The solution is to use backtick and carefully unquote/quote the symbols which should remain unqualified.

```clojure
(deftest Foo 
  (testing "works fine in REPL, but throws an error in lein test"
    (eval '(deflow foo [] (<*))))

  (testing "works in both REPL and lein test"
    (eval `(deflow ~'foo [] (<*)))))
```
