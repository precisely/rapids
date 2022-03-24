(ns rapids.partitioner.partition
  (:require [rapids.partitioner.partition-utils :refer [dynamic?]])
  (:import (clojure.lang PersistentVector)))

;;;
;;; A Partition is a signature which will be used to create a partition function
;;;
(defrecord Partition
  [^PersistentVector params
   ^PersistentVector body
   ^Boolean final])

(defn ->partition
  ([params body] (->partition params body false))
  ([params body final]
   {:pre [(sequential? params) (vector? body)]}
   (->Partition (vec params) body (boolean final))))

(defn partition? [o] (instance? Partition o))

(defn partition-fn-def
  "Returns the code which defines the partition fn at address"
  [pmap address index]
  (letfn [(body-with-dynamic-bindings [bindings body]
            (if (empty? bindings) body
              `((binding ~bindings ~@body))))]
    (let [cdef             (get pmap address)
          name             (symbol (str (name (:flow address)) index))
          params           (:params cdef)
          dynamics         (filter dynamic? params) ; TODO: disallow binding system dynamic vars - security issue
          dynamic-bindings (vec (flatten (map #(vector % %) dynamics)))]
      `(fn ~name [{:keys ~params}]
         ~@(body-with-dynamic-bindings dynamic-bindings (:body cdef))))))
