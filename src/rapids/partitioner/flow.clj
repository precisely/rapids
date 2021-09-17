(ns rapids.partitioner.flow
  (:require [rapids.partitioner.partition :as p]
            [rapids.partitioner.partition-set :as pset]
            [rapids.partitioner.partition-utils :as putil]
            [rapids.objects.flow :as flow]
            [rapids.objects.address :as address]
            [rapids.support.util :refer [qualify-symbol reverse-interleave]]))

(declare params-from-args extract-signatures partition-signature)

(defn partition-flow
  "Arguments:
  m - meta data, containing the :line of the flow definition
  address - starting address of the flow
  fdecl - the flow declaration, of the form:
           (([arglist1...] body1...) ([arglist2...] body2...)...)
           or
           ([argslist...] body...)
  Returns:
  [entry-fn-def, pset]

  entry-fn-def - a list of the form (fn ([...] ...) ([...] ..) ...)"
  [m address fdecl]
  (let [sigs (extract-signatures fdecl)
        entry-point-name (str (address/to-string address) "__entry-point")
        [psets, arity-defs] (reverse-interleave
                              (apply concat
                                (map-indexed
                                  (fn [idx sig]
                                    (partition-signature m (address/child address idx) sig))
                                  sigs))
                              2)
        pset (apply pset/combine psets)
        entry-fn-def `(fn ~(symbol entry-point-name) ~@arity-defs)]
    [entry-fn-def, pset]))

(defn- partition-signature
  "Returns a pset and an arity definition.

  Arity definition is a list containing an argument vector and code body"
  [m address sig]
  (let [[args & code] sig
        params (params-from-args args [] (-> m :line))
        [start-body, pset, _] (p/partition-body (vec code) address address params)
        pset (pset/add pset address params start-body)
        entry-continuation-bindings (putil/bindings-expr-from-params params)]
    [pset, `([~@args] (flow/exec ~address ~entry-continuation-bindings))]))

;;
;; HELPERS
;;

(defn- params-from-args
  "given an argument vector, returns a vector of symbols"
  [args params line]
  (let [arg (first args)]
    (cond
      (= arg '&) (recur (rest args) params line)
      (map? arg) (recur (rest args) (concat params (:keys arg)) line)
      (symbol? arg) (recur (rest args) (conj params arg) line)
      (nil? arg) (vec params)
      :else (throw (ex-info (str "Unexpected argument " arg)
                     {:type :compiler-error})))))

(defn- extract-signatures [fdecl]
  (let [[_ _ & sigs] (macroexpand `(fn random-name# ~@fdecl))]
    sigs))