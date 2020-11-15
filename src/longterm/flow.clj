(ns longterm.flow
  (:require [longterm.address :as address]
            [longterm.util :refer [refers-to?]]
            [longterm.defrecordfn :refer [defrecordfn]])
  (:import (longterm.address Address)))

(defrecordfn Flow
  [;; Global symbol defined as this flow
   name
   ;; Function with arbitrary signature
   entry-point
   ;; A map of address-point strings to functions of the form (fn [& {:keys [...]}])
   continuations
   ;; For debugging purposes:
   partitions]
  (fn [this & _] (throw (Exception. (str "Attempt to invoke flow " (:name this) " outside of run context."))))

  Object
  (toString [_] (format "#<Flow %s (%d partitions)>" name (count continuations))))

(defn flow? [o]
  (instance? Flow o))

(defn exec
  "Executes the flow partition at the address with the given bindings"
  ([address bindings]
   {:pre [(instance? Address address)
          (map? bindings)]}
   (let [flow (address/resolved-flow address)
         continuation (get-in flow [:continuations address])
         args (apply concat (map (fn [[key val]] [(keyword key) val]) bindings))]
     (if-not (fn? continuation)
       (throw (Exception. (format "Attempt to continue flow at undefined partition %s" address))))
     (apply continuation args))))

(defn entry-point
  [flow args]
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
