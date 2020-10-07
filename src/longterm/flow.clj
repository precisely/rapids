(ns longterm.flow
  (:require [longterm.address :as address]
            [longterm.util :refer [refers-to?]])
  (:import (longterm.address Address)))

(defrecord Flow
  [;; Global symbol defined as this flow
   name
   ;; Function with arbitrary signature
   entry-point
   ;; A map of address-point strings to functions of the form (fn [& {:keys [...]}])
   continuations
   ;; For debugging purposes:
   partitions])

(defn flow? [o]
  (instance? Flow o))

(defn continue
  ([address bindings]
   {:pre [(instance? Address address)
          (map? bindings)]}
   (let [flow (address/resolved-flow address)
         continuation (get-in flow [:continuations address])
         args (apply concat (map (fn [[key val]] [(keyword key) val]) bindings))]
     (apply continuation args))))
  ;([flow point bindings]
  ; {:pre [(flow? flow)
  ;        (vector? point)
  ;        (map? bindings)]}
  ; (let [continuation (-> flow :continuations address)
  ;       ]
  ;  (apply continuation continuation-args))))

(defn start
  [flow & args]
  (cond
    (flow? flow) (apply (get flow :entry-point) args)
    (symbol? flow) (recur (resolve flow) args)
    (var? flow) (recur (var-get flow) args)
    :else (throw (Exception. (format "Invalid flow %s" flow)))))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) ">")
    w))
