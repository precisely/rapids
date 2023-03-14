# Rapids
[![dev](https://github.com/precisely/rapids/actions/workflows/tests.yml/badge.svg?branch=dev&label=dev&text=dev%20tests&title=dev%20tests)](https://github.com/precisely/rapids/actions/workflows/tests.yml)

A DSL for programming user interaction flows. 

Rapids defines a new macro, `deflow`, akin to `defn`, but which  permits suspending execution until an external event is received. This is done with the `(input!)` special form. The system uses a persistent Storage which saves the state of the computation when the `input!` operator is invoked. An in-memory and Postgres implementation are provided. 

The control API consists of two main functions, `start!` and `continue!` for starting and continuing flows. 

## Basic Usage
Also see `tests/rapids_test.clj`.

### Define a flow
```clojure
(deflow multiply-by-user-input [x]
  (*> "Hi, please enter a number!")
  (let [user-num, (Integer/parseInt (<*))
        result (* user-num x)]
  (*> (str "Multiplying " x " by " user-num " gives " result))  
```
As of 0.3.2, `deflow` supports multi-arity signatures and pre/post conditions like `defn`.

### Simple example
```clojure
(let [run (start! multiply-by-user-input [5])]
   (continue! (:id run) {:input "100"}) ; this would normally happen as the result of a separate web API call
   (println (:result run))) ; prints 500
=> 
```

#### output! (shorthand: *>)

```
(output! arg*) ; or (*> arg*)
```
Write an object to the `:output` key.

The `output!` operator is conceptually akin to writing to stdout, but the output is collected and returned as the `:output` key of a run object returned by `start!` or `continue!` when a flow hits a `input!` or completes execution.

`output!` take an arbitrary number of objects which are appended to the current response vector. Note that the response vector is automatically cleared before a run is continued so each request only retrieves some of the `output!` arguments in a flow.

#### input! (shorthand: <*)

Suspends execution of the run until a call to `continue!`.
 
```
(input!) ; or (<*)
(input! :permit permit)
(input! :expires expiry-time, :default value)
(input! :permit permit, :expires expiry-time, :default value)
```

A call to `input!` causes the run to be persisted to storage. Execution is resumed by calling `continue!` and providing the run-id (available using `(:id run)`, the permit value (which is nil by default) and `input` value. When the run resumes, the `(input!...)` form evaluates to the `result` value. 

When the expiry time is passed, execution resumes, with the `input!` operator evaluates to the value of the `default` argument, which is nil if not provided.

#### wait-for!

Suspends execution of the current run until the given run completes. Returns the value returned by the given run. 

```
(wait-for! run) 
(wait-for! run :expires expiry-time, :default value) 
```

#### set-index! 

Sets one or more values in the current run's hierarchical index. Key value pairs are provided. Keys may be vectors, indicating a nested value.

```clojure
(set-index! :foo 1, [:a :b] 2) 
(current-run :index) ; => {:foo 1 {:a {:b 2}}}
```

### Starting a flow

```clojure
;; start! creates a run, beginning execution of the given flow

(start! multiply-by-user-input [4])
  ...
```

### Resume the flow 
```clojure
;; the caller provides run-id, permit and input 
;; resume the flow as follows:
(continue! run-id {:permit permit :input input})
;; the value provided to input is returned by (input!)
```
### Getting a specific run
```clojure
(get-run run-id)
```

### Querying for runs
The `find-runs` API allows for queries on multiple fields and JSON subfields of a run.
```clojure
;; query for runs which are running wheere a nested index key has a particular value
(find-runs [[:state :eq :running] 
            [[:index :runs :patient :initial-labs] :eq lab-run-id]] 
{:limit 3})
```
## Setting up a backend

Rapids works by saving the runtime state in non-volatile storage. This capability can be provided by implementing the protocols, in rapids.storage.protocol: Storage and StorageConnection. The library contains implementations of an in memory implementation (used for testing) and a Postgres-based implementation.

### Set up a local PostgresStorage Backend
#### Installing Postgresql
We use:
```shell
brew install postgresql
# To start postgresql for the first time:
brew services start postgresql
# To restart postgresql after an upgrade:
brew services restart postgresql
#Or, if you don't want/need a background service you can just run:
/usr/local/opt/postgresql/bin/postgres -D /usr/local/var/postgres

```
#### Create databases
```shell
createdb rapids-test 
createdb rapids_storage 
```

#### Add code to your application 
```clojure
(ns mynamespace
  (:require [rapids :refer :all]
            [rapids.implementations.postgres-storage :refer [->postgres-storage postgres-storage-migrate!]])
(set-storage! (->postgres-storage {:jdbcUrl "jdbc:postgresql://localhost:5432/rapids_storage`}))
(postgres-storage-migrate!) ; uses the top-level storage by default          
```
## Environment variables

1. Install [direnv](https://direnv.net/)
2. Copy `.envrc.example` to `.envrc`
3. Edit `.envrc` to set the environment variables 
4. Run `direnv allow` to allow the environment variables to be set

If you're using IntelliJ, the EnvFile plugin can be helpful for getting variables into your REPL:
https://plugins.jetbrains.com/plugin/7861-envfile

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
  (branch [run (start! welcome)]
    "welcome" 
    (keys-match run
      :state :suspended
      :output ["welcome. Do You want to continue?" _])

    (branch [run (continue! (:next-id run) {:input "yes"})]
      "wants to continue"
      (keys-match run
        :state :suspended 
        :output ["great!... let's continue"]))

    (branch [run (continue! (:next-id run) {:input "no"})]
      "doesn't want to continue"
      (keys-match run
        :state :complete))))
```

### branch

Creates nested test conditions.

```clojure
(branch [...bindings] description & body)
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
* `:input-error` - invalid input was provided to the system. A run does NOT move to :error state and the error is returned to the caller

## Coverage

There's currently a problem in using cloverage with deflow. It seems that cloverage instruments the keys in `:partition-fns`, which are addresses, by associng new keys. This results in call-partition failing because the internal addresses differ from the requested addresses (which aren't instrumented). 

The current solution is just ot exclude the file(s) which have deflow. As of time of writing, this is only rapids.language.cc. Obviously, we need a better solution for the future - maybe by implementing a custom class of Address which cannot be instrumented.
```shell
lein cloverage --lcov --exclude-call rapids.language.cc/make-current-continuation --ns-exclude-regex 'rapids\.language\.cc'
```

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

   You need to provide access key id & secret to push to the S3 bucket. You will need write access, obviously. Set environment variables. Recommended approach is to use direnv. Leiningen needs these variables to access resources (e.g., AWS credentials), but there is not a clean way of loading them using lein plugins. 
   
   ```shell
   brew install direnv
   cp .envrc.sample .envrc
   # edit .envrc to set the variables
   direnv allow
   ```
   
8. Deploy the library to S3

  ```shell script
lein deploy precisely
  ```

### Local Deployment

During development, you may want to publish to a local repo instead of to S3.  This can be done by publishing to your local Maven repo. This is typically at `~/.m2`.  The dev profile has the [lein-localrepo plugin](https://github.com/kumarshantanu/lein-localrepo) installed.

First build your target:

```
lein build
``` 

### Contributing

1. Commit new code to a branch under your name. E.g., `aneil/my-new-feature`
2. Issue a PR requesting a merge into `dev`
3. Wait for automated tests to complete on Github
4. Merge on Github (not locally) 

#### Publishing 

1. Merge dev into master locally  
  ```shell
  git co dev # alias for checkout
  git pull 
  lein test # make sure all tests are passing!
  git co master
  git merge dev
  ```
2. Bump version in project.clj
3. Tag and push to master
  ```shell
  git tag <<VERSION-NUMBER>> # eg., git tag 1.0.1
  git po # alias for !git push -u origin `git branch --show-current` 
  git pt # alias for push --tags
  ```
4. Publish repo
  ```shell
  lein deploy precisely
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
