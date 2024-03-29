# TODO

* must implement case*, throw, set!, also throw/catch/finally - but disallow input!
* enable recursive deflow calls (it currently will fail because flow being defined is not recognized as a flow)
* figure out parallel flow mechanism. Initial ideas: 
    * channels - separate table in RunStore (maybe rename to LongTermStore)
        - structure might be 
          #channel{:queue [] :depth n :address <> :result-key 'foo :waiting true}
         - queue - the values, depth how many values may be stored
         - mode - fifo, ordered-set, filo, prioritized
         - address - the partition to continue at
         - result-key - the binding value of the result
         - waiting - marked true when receiver is halted and waiting for results from channel
    * globals - provided to start! and accessible to all runs and sub-runs
         - e.g., may want to store a global channel representing user-activities
    * launch, wait & resume subflows according to the input context
      `(defer-while [:user] run) ; defers control to run while it remains waiting for input in :user context`
        - returns either run waiting for input in alternate state or  
        - alternates: 
                * `(wait-while [:user] run)` 
                * `(wait-on run :user)` 
                * `(cede! [:user] run)`
                * `(cede! run :user)`
          perhaps we start the process with `(wait-on (start! flow [1 2 3]) :user)`
* add expiry mechanism
  - an API on the runstore - expired-suspensions, plus index the expiry 
* add error handling
  - add :error state, and error fields to Run 
* add multi-arity support to deflow
* spec out versioning 
* add support for try/catch
* add support for fn/letfn
    - may require generalized support for object persistence across runlets
* consider constraint mechanism for acquire - (fn [input, event] ) => boolean
  - or perhaps raise error to indicate reason for failure
* finish loop tests in Rapids_test.clj 
* documentation
