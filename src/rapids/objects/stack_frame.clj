(ns rapids.objects.stack-frame
  (:require [rapids.objects.address :refer [address?]]
            [rapids.objects.flow :as flow]))

(defrecord StackFrame [address bindings input-key])
(defn stack-frame? [o] (instance? StackFrame o))

(defn make-stack-frame
  [address bindings input-key]
  {:pre [(address? address)
         (map? bindings)
         (or (nil? input-key)
           (and (symbol? input-key)
             (not (qualified-symbol? input-key))))]}
  (StackFrame. address, bindings, (keyword input-key)))

(defn stack-fn
  "Returns a function `(fn [input] ...)` which calls a partition fn with the bindings
  provided by the stack frame and ensures the value of `input` is bound to stack frame's input-key.
  This is how external events transmit a value into a point of a flow."
  [frame]
  {:pre [(stack-frame? frame)]}
  (let [address    (:address frame)
        input-key (:input-key frame)
        bindings   (:bindings frame)]
    (fn [input]
      (let [bindings-with-input (if input-key
                                   (assoc bindings
                                     input-key input)
                                   bindings)]
        (flow/call-partition address bindings-with-input)))))
