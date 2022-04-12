(ns rapids.partitioner.closure
  (:require [clojure.spec.alpha :as s]
            [rapids.partitioner.node :as n]
            [rapids.objects.closure :refer [->Closure]]
            [rapids.partitioner.partition-utils :refer :all]
            [rapids.support.util :refer :all]
            [rapids.objects.address :as a]))

(declare extract-fn-defs)
(s/def ::params (s/* unqualified-symbol?))

(defn closure-node
  "Given an expr (fn-form) which constructs a function and lexical parameters available for binding,
  returns:
  a node representing the closure"
  [fn-form address env-params]
  {:pre [(s/assert ::params env-params)]}
  (let [[_, fndefs] (extract-fn-defs fn-form)
        params (map first fndefs)
        bodies (map rest fndefs)
        captured-params (closure-captured-bindings params, bodies, env-params)
        entry-address (a/child address '_fn)
        closure-ctor (with-meta `(->Closure ~entry-address ~(bindings-expr-from-params captured-params) false) (meta fn-form))]
    (-> (n/->valued-node address env-params [closure-ctor])
      (n/add-partition entry-address captured-params [fn-form] :valued))))

(defn extract-fn-defs [form]
  "Returns [name, sigs]"
  (let [[_ & sigs] form
        name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs))
               (list sigs)
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (IllegalArgumentException.
                          (if (seq sigs)
                            (str "Parameter declaration "
                              (first sigs)
                              " should be a vector")
                            (str "Parameter declaration missing"))))))]
    [name, sigs]))


