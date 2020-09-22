(ns longterm.thunk
  (:require [longterm.flow :as flow]
            [longterm.address :as address]))

;;;; Continuation

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


(defrecord Thunk
  [address
   bindings
   result-key])                                             ; a symbol or vector of symbols

(declare bindings-to-args bindings-with-result)

(defn resume-at
  "Generates code that continues execution at address after flow-form is complete.
  address - names the continuation
  params - list of parameters needed by the continuation
  result-key - the key to which the value of form will be bound in the continuation
  flow-form - expression which invokes a flow"
  ([[address params] flow-form]
   (resume-at [address params nil] flow-form))

  ([[address params result-key] flow-form]
   `(let [bindings# (hash-map ~@(flatten (map (fn [p] `('~, p, p)) params)))
          new-thunk# (Thunk. ~address bindings# '~result-key)]
      (binding [*thunks* (cons new-thunk# *thunks*)]
        ~flow-form))))

(defn return-with
  "Returns a result to the thunk at the top of the stack"
  ([thunks result]
   (binding [*thunks* thunks] (return-with result)))

  ([result]
   (let [[thunk remaining-thunks] *thunks*                  ; pop the top thunk (*thunks* is bound to rest)
         address (-> thunk :address)
         point (-> address :point)
         flow (-> address :flow (resolve) (symbol))
         bindings (bindings-with-result thunk result)]
     (binding [*thunks* remaining-thunks]
       (flow/continue flow point ~@(bindings-to-args bindings))))))


;;
;; Helpers
;;
(defn- bindings-to-args
  "Takes a map of bindings and returns the corresponding keyword argument list"
  [bindings]
  (flatten (map #((list (keyword %1) %2)) (seq bindings))))

(defn- make-multiple-value-args
  [bindings result-keys results]
  (if-not (and (seq? results)
            (= (count result-keys) (count results)))
    (throw (Exception. (format "Expecting multiple value result matching keys %s, but received %s"
                         result-keys results))))
  (apply assoc bindings (interleave result-keys results)))

(defn- bindings-with-result [thunk result]
  (let [result-key (-> thunk :result-key)
        bindings (-> thunk :bindings)]
    (cond
      (seq? result-key) (make-multiple-value-args bindings result-key result)
      (symbol? result-key) (assoc bindings result-key result)
      bindings)))

;
;(declare deflow myflow z normal fn1 fn2 flow1 flow2 flow3 flow4)
;
;(deflow myflow [z]
;  (fn2 (fn1
;         (if (normal)
;           (flow1 (do (flow2) true) (flow3))
;           (flow4 2))
;         (fn2)
;         (flow1))))

;; p1:  (if (normal)
;;         (do (resume-at [p2 [z]] (flow2)))
;;         (resume-at [p4 [z]] (flow4 2)))
;; p2:  (let [fn1-arg0 true
;;            fn1-arg1 (fn2)]
;;        (resume-at [p3 [z fn1-arg1] fn1-arg2 (flow1)))
;; p3:  (fn1 fn1-arg0 fn1-arg1 fn1-arg2)