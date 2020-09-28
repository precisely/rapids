(ns longterm.stack
  (:require [longterm.flow :as flow]
            [longterm.address :as address]))

;;;; Continuation

;;; Execution passes between continuations using stack frames.
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

(def ^:dynamic *stack* ())

(def ^:const SUSPEND ::SUSPEND)

(defrecord StackFrame
  [address
   bindings
   result-key
   event-id
   expiry])                             ; a symbol or vector of symbols

(declare bindings-to-args bindings-with-result result-with)

(defn suspend-signal? [x] (= x SUSPEND))

(defmacro resume-at
  "Generates code that continues execution at address after flow-form is complete.
  address - names the continuation
  params - list of parameters needed by the continuation
  result-key - the key to which the value of form will be bound in the continuation
  body - expression which invokes a flow"
  [[address params result-key event-id expiry] & body]
  (assert address)
  (assert (vector? params))
  `(let [bindings#  (hash-map ~@(flatten (map (fn [p] `('~p p)) params)))
         new-frame# (StackFrame. ~address bindings# '~result-key ~event-id ~expiry)]
     (binding [*stack* (cons new-frame# *stack*)]
       ~@body)))

(defn return-with
  "Returns a result to the frame at the top of the stack"
  ([frames result]
   (binding [*stack* frames] (return-with result)))

  ([result]
   (let [[frame remaining-frames] *stack* ; pop the top frame (*frames* is bound to rest)
         address  (-> frame :address)
         point    (-> address :point)
         flow     (-> address :flow (resolve) (symbol))
         bindings (bindings-with-result frame result)]
     (binding [*stack* remaining-frames]
       (flow/continue flow point ~@(bindings-to-args bindings))))))

;;
;; Helpers
;;
(defn- bindings-to-args
  "Takes a map of bindings and returns the corresponding keyword argument list"
  [bindings]
  (flatten (map #((list (keyword %1) %2)) (seq bindings))))

;(defn- make-multiple-value-args
;  [bindings result-keys results]
;  (if-not (and (seq? results)
;            (= (count result-keys) (count results)))
;    (throw (Exception. (format "Expecting multiple value result matching keys %s, but received %s"
;                         result-keys results))))
;  (apply assoc bindings (interleave result-keys results)))

;(defn- bindings-with-result [frame result]
;  (let [result-key (-> frame :result-key)
;        bindings   (-> frame :bindings)]
;    (cond
;      (seq? result-key) (make-multiple-value-args bindings result-key result)
;      (symbol? result-key) (assoc bindings result-key result)
;      :else bindings)))

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