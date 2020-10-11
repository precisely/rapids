# longterm

A DSL for programming long running flows. 

Longterm defines a new macro, `deflow`, akin to `defn`, but which  permits suspending execution until an external event is received. This is done with the `(suspend! event-id)` special form. The system uses a user-definable RunStore which stores the state of the computation when a `suspend!` is encountered. A default in memory runstore is provided, but the system is intended to be used with persistent storage. 

Execution is restarted using `(process-event! run-id event-id optional-result)`. The result provided to `process-event!` becomes the value of `suspend!` expression in the ensuing computation, which continues until complete or another `suspend!` is encountered.  

## Basic Usage

See tests/longterm_test.clj.

## License

