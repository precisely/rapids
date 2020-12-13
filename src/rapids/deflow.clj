(ns rapids.deflow
  (:require [rapids.address :as address]
            rapids.run-loop
            [rapids.util :refer [qualify-symbol reverse-interleave]]
            [rapids.partition :as p]
            [rapids.flow :as flow]
            [rapids.partition-utils :refer [bindings-expr-from-params]]
            [rapids.partition-set :as pset])
  (:import (rapids.flow Flow)))

(declare params-from-args params-to-continuation-args expand-flow partition-signature)

(defmacro deflow
  "Define a long term flow which suspends execution at (suspend ...) expressions."
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [name docstring? & fdecl]
  (if-not (string? docstring?)
    (with-meta `(deflow ~name "" ~docstring? ~@fdecl) (meta &form))
    `(def ^{:doc ~docstring?} ~name ~(expand-flow (meta &form) name fdecl))))

(defn extract-signatures [name fdecl]
  (let [[_ _ & sigs] (macroexpand `(fn ~name ~@fdecl))]
    sigs))

(defn expand-flow [m name fdecl]
  (binding [flow/*defining-flows* (conj flow/*defining-flows* (qualify-symbol name))]
    (let [sigs (extract-signatures name fdecl)
          qualified (qualify-symbol name)
          address (address/create qualified)
          entry-point-name (str name "__entry-point")

          [psets, arity-defs] (reverse-interleave
                                (apply concat
                                  (map-indexed
                                    (fn [idx sig]
                                      (partition-signature m (address/child address idx) sig))
                                    sigs))
                                2)
          pset (apply pset/combine psets)
          entry-fn-def `(fn ~(symbol entry-point-name) ~@arity-defs)]

      `(let [cset# ~(pset/continuation-set-def pset)]
         (Flow. '~qualified, ~entry-fn-def, cset#, ~pset)))))


(defn partition-signature
  "Returns a pset and an arity definition"
  [m address sig]
  (let [[args & code] sig
        params (params-from-args args [] (-> m :line))
        [start-body, pset, _] (p/partition-body (vec code) address address params)
        pset (pset/add pset address params start-body)
        entry-continuation-bindings (bindings-expr-from-params params)]
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
