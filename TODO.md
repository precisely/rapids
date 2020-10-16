# TODO

* figure out parallel flow mechanism. Initial ideas: 
    * channels - separate table in RunStore (maybe rename to LongTermStore)
        - structure might be 
          #channel{:queue [] :depth n :address <> :result-key 'foo :ready true/false}
         - queue - the values, depth how many values may be stored
         - mode - fifo, ordered-set, filo, prioritized
         - address - the partition to continue at
         - result-key - the binding value of the result
         - ready - marked true when receiver is halted and waiting for results from channel
    * globals - provided to start-flow! and accessible to all runs and sub-runs
         - e.g., may want to store a global channel representing user-activities
    * launch, wait & resume subflows according to suspend context
      ... the suspend "context" is what is currently the event-id
      (defer-while [:user] run) ; defers control to run while it remains suspended in :user context
        - returns either run suspended in alternate state or  
        - alternates: (wait-while [:user] run) or (wait-on run :user)
          perhaps we start the process with `(wait-on (start-flow! flow 1 2 3) :user)`
* add expiry mechanism
  - an API on the runstore - expired-suspensions, plus index the expiry 
* add error handling
  - add :error state, and error fields to Run 
* test macroexpansion
* add multi-arity support to deflow
* spec out versioning 
* add support for try/catch
* add support for fn/letfn
    - may require generalized support for object persistence across runlets
* consider constraint mechanism for unsuspend - (fn [suspend, event] ) => boolean
  - or perhaps raise error to indicate reason for failure 
* documentation
