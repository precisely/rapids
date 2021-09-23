(ns rapids.objects.stack-frame
  (:require [rapids.objects.address :refer [address?]]
            [rapids.objects.flow :as flow]))

(defrecord StackFrame [address bindings data-key])
(defn stack-frame? [o] (instance? StackFrame o))

(defn make-stack-frame
  [address bindings data-key]
  {:pre [(address? address)
         (map? bindings)
         (or (nil? data-key)
           (and (symbol? data-key)
             (not (qualified-symbol? data-key))))]}
  (StackFrame. address, bindings, (keyword data-key)))

(defn stack-fn
  "Returns a function `(fn [data] ...)` which calls a partition fn with the bindings
  provided by the stack frame and ensures the value of `data` is bound to stack frame's data-key.
  This is how external events transmit a value into a point of a flow."
  [frame]
  {:pre [(stack-frame? frame)]}
  (let [address    (:address frame)
        data-key (:data-key frame)
        bindings   (:bindings frame)]
    (fn [data]
      (let [bindings-with-data (if data-key
                                   (assoc bindings
                                     data-key data)
                                   bindings)]
        (flow/call-partition address bindings-with-data)))))
