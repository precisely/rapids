(ns longterm.flow
  (:import (clojure.lang IFn Symbol IPersistentVector)))

(defprotocol IFlow
  (flow-exec [flow n args]))

(defrecord Flow
  [^Symbol name
   ^String hash
   ^IFn entry-point
   ^IPersistentVector thunks]
  IFlow
  (flow-exec [flow n args] (apply (-> flow :thunks (nth n)) args))
  IFn
  (invoke [flow & rest] (apply (:entry-point flow) rest)))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) " >")
    w))
