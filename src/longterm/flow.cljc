(ns longterm.flow
  (:import (clojure.lang Symbol IFn IPersistentMap)))

(declare flow?)

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
  [flow & args]
  (cond
    (symbol? flow) (recur (resolve flow) args)
    (var? flow) (recur (var-get flow) args)
    (flow? flow) (apply (get flow :entry-point) args)
    :else (throw (Exception. "Invalid flow %s" flow))))

(defn flow?
  "True if o is a Flow instance or a symbol or Var pointing to a flow"
  [o]
  (instance? o Flow))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) " >")
    w))
