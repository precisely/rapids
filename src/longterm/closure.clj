;;;
;;; Enables persisting a closure across partitions. The strategy is to store the
;;; function as a continuation in the Flow :continuations,
;;;
(ns longterm.closure
  (:require [clojure.spec.alpha :as s]
            [longterm.defrecordfn :refer [defrecordfn]]
            [longterm.flow :as flow]
            [longterm.util :refer [in? unqualified-symbol?]]
            [longterm.partition-utils :refer :all]
            [longterm.partition-set :as pset]))

(declare closure-bindings)

(defrecordfn Closure [address bindings]
  (fn [& args] (apply (flow/exec address bindings) args)))

(s/def ::fn-form (s/and seq? #(in? '[fn fn*] %)))
(s/def ::params (s/* unqualified-symbol?))

(defn closure? [o] (instance? Closure o))

(declare extract-fn-defs)
(defn closure-constructor
  "Given an expr (fn-form) which constructs a function and lexical parameters available for binding,
  returns:
  [closure-ctor, pset]
  closure-ctor expression that returns a Closure within a de& body.
  continuation-def is a definition of a continuation function which returns a closure
  of the fn defined by expr based on the current lexical bindings."
  [fn-form address env-params]
  {:pre  [(s/assert ::fn-form fn-form)
          (s/assert ::params env-params)]
   :post [(s/assert [::fn-form, ::fn-form] %)]}
  (let [[_, fndefs] (extract-fn-defs fn-form)
        params          (map first fndefs)
        bodies          (map rest fndefs)
        captured-params (closure-captured-bindings params, bodies, env-params)
        closure-ctor    `(->Closure ~address ~(bindings-expr-from-params captured-params))

        pset            (pset/add (pset/create) address captured-params [fn-form])]
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


