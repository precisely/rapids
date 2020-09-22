(ns longterm.flow
  (:import (clojure.lang Symbol IFn IPersistentMap)))

(defrecord Flow
  [; Global symbol defined as this flow
   name
   ; Function with arbitrary signature
   entry-point
   ; A map of address-point strings to functions of the form (fn [& {:keys [...]}])
   continuations])

(defn continue
  [flow point bindings]
  (apply (-> flow :continuations point) bindings))

(defn start
  [flow args]
  (apply (get flow :entry-point) args))

(defn flow?
  "True if o is a Flow instance or a symbol or Var pointing to a flow"
  [o]
  (case (type o)
    Symbol (flow? (var-get (resolve o)))
    Var    (flow? (var-get o))
    Flow   true))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) " >")
    w))
