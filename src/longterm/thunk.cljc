(ns longterm.thunk
  (:require [longterm.flow :as flow]))

;;;; Thunks

;;; Execution passes between continuations using thunks.
;;; Thunks provide the means for capturing and transmitting the bindings
;;; between continuations.

;;; For example in the following:
;;; ```
;;; (let [foo (myflow)]
;;;   (use-foo foo))
;;; ```
;;; The two calls above are actually executed in two separate functions ("continuations").
;;; The second line of code would be in a continuation that looks like this:
;;; ```(fn [& {:keys [foo]}] (use-foo foo))```
;;; The call to `(myflow)` causes a thunk to be generated which captures any existing bindings,
;;; the variable that the result of `(myflow)` should be bound to, and the continuation that
;;; needs to be invoked. At some point, the `(myflow)` call, perhaps in a nested call to some
;;; other flow, stops at a `(wait-for-event ...)` call. This function associates an event with
;;; a thunk, and persists this information in the datastore provided in *thunk-store*. When an event arrives, it is
;;; matched

;;;

(def ^:dynamic *thunks* ())

(defrecord Thunk [continuation-id bindings result-key])

(declare bindings-to-args)

(defmacro with-thunk
  "Push a new thunk onto the stack"
  [[continuation-id bindings result-key] & code]
  (let [new-thunk (Thunk. ~continuation-id ~bindings ~result-key)]
    `(binding [*thunks* (cons new-thunk *thunks*)]
       ~@code)))

(defn thunk-return
  "Returns a result to the thunk at the top of the stack"
  ([thunks result]
   (binding [*thunks* thunks] (thunk-return result)))

  ([result]
   (let [[thunk remaining-thunks] *thunks*                  ; pop the top thunk
         [flow-name hash n] (:continuation-id thunk)        ; in future, use the hash to get correct version
         flow (resolve (symbol flow-name))
         result-key (:result-key thunk)
         bindings (assoc (:bindings thunk) result-key result)]
     (binding [*thunks* remaining-thunks]
       (flow/flow-continue flow n ~@(bindings-to-args bindings))))))

;;
;; Helpers
;;
(defn bindings-to-args
  "Takes a map of bindings and returns the corresponding keyword argument list"
  [bindings]
  (flatten (map #((list (keyword %1) %2)) (seq bindings))))