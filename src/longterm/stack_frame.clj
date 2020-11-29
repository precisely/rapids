(ns longterm.stack-frame
  (:require [longterm.flow :as flow]))

(defrecord StackFrame [address bindings result-key])
(defn stack-frame? [o] (instance? StackFrame o))

(defn make-stack-frame
  [address bindings result-key]
  {:pre [(longterm.address/address? address)
         (map? bindings)
         (or (nil? result-key)
           (and (symbol? result-key)
             (not (qualified-symbol? result-key))))]}
  (StackFrame. address, bindings, (keyword result-key)))

(defn stack-continuation
  "Returns a function `(fn [value] ...)` which calls a flow continuation with the bindings
  provided by the stack frame, plus a `value` which will be bound to the result-key
  provided in `(resume-at ...)`. This is how external events transmit a value into a point
  a flow."
  [frame]
  {:pre [(stack-frame? frame)]}
  (let [address    (:address frame)
        result-key (:result-key frame)
        bindings   (:bindings frame)]
    (fn [result]
      (let [bindings-with-result (if result-key
                                   (assoc bindings
                                     result-key result)
                                   bindings)]
        (flow/exec address bindings-with-result)))))
