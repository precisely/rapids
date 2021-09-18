(ns rapids.partitioner.closure
  (:require [clojure.spec.alpha :as s]
            [rapids.objects.closure :refer :all]
            [rapids.partitioner.partition-utils :refer :all]
            [rapids.partitioner.partition-set :as pset]
            [rapids.support.util :refer :all]))

(declare extract-fn-defs)
(s/def ::params (s/* unqualified-symbol?))

(defn closure-constructor
  "Given an expr (fn-form) which constructs a function and lexical parameters available for binding,
  returns:
  [closure-ctor, pset]
  closure-ctor expression that returns a Closure within a de& body.
  continuation-def is a definition of a continuation function which returns a closure
  of the fn defined by expr based on the current lexical bindings."
  [fn-form address env-params]
  {:pre [(s/assert ::params env-params)]}
  (let [[_, fndefs] (extract-fn-defs fn-form)
        params (map first fndefs)
        bodies (map rest fndefs)
        captured-params (closure-captured-bindings params, bodies, env-params)
        closure-ctor `(->Closure ~address ~(bindings-expr-from-params captured-params) false)

        pset (pset/add (pset/create) address captured-params [fn-form] true)]
    [closure-ctor, pset]))

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


