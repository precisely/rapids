;;;
;;; Enables persisting a closure across partitions. The strategy is to store the
;;; function as a continuation in the Flow :continuations,
;;;
(ns longterm.closure
  (:require [clojure.spec.alpha :as s]
            [longterm.defrecordfn :refer [defrecordfn]]
            [longterm.flow :as flow]
            [longterm.util :refer [in? unqualified-symbol?]]
            [longterm.partition-utils :refer :all]))

(declare closure-bindings)

(defrecordfn Closure [address bindings]
  (fn [& args] (apply (flow/exec address bindings) args)))

(s/def ::fn-form (s/and seq? #(in? '[fn fn*] %)))
(s/def ::params (s/* unqualified-symbol?))

(defn closure? [o] (instance? Closure o))

(defn partition-closure
  "Given an expr (fn-form) which constructs a function and lexical parameters available for binding,
  returns:
  [closure-expr, continuation-def]
  closure-expr returns a Closure within a deflow body.
  continuation-def is a definition of a continuation function which returns a closure
  of the fn defined by expr based on the current lexical bindings."
  [fn-form address params]
  {:pre  [(s/assert ::fn-form fn-form)
          (s/assert ::params params)]
   :post [(s/assert [::fn-form, ::fn-form] %)]}
  (let [captured-params           (params-used-by fn-form params)
        closure-continuation-def `(fn [{:keys ~captured-params}] ~fn-form)
        closure-def              `(->Closure address ~(bindings-expr-from-params captured-params))]
    [closure-def, closure-continuation-def]))
