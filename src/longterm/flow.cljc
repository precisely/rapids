(ns longterm.flow
  (:import (clojure.lang Symbol IPersistentVector)))

(defprotocol IFlow
  (flow-continue [flow n args])
  (flow-start [flow & args))

(defrecord Flow
  [^Symbol name
   ^String hash
   ^IFn entry-point
   ^IPersistentVector continuations]
  IFlow
  (flow-continue [flow n bindings] (apply (-> flow :continuations (nth n)) bindings))
  (flow-start [flow args] (apply (-> flow :entry-point) args)))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) " >")
    w))
