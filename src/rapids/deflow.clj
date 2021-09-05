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

(defn expand-flow
  "Returns an expression for constructing a Flow from arguments provided as: (deflow ^m name & fdecl)

  m - meta data
  name - symbol naming the flow
  fdecl - the body of deflow, where fdecl is a list of one or more elements of the form:
            [(arglist1 code-body1), (arglist2 code-body2)...].
            Each (arglistn code-bodyn) is called an arity"
  [m name fdecl]
  (binding [flow/*defining-flows* (conj flow/*defining-flows* (qualify-symbol name))]
    (let [sigs (extract-signatures name fdecl)
          qualified (qualify-symbol name)
          address (address/create qualified)
          entry-point-name (str name "__entry-point")

          ;; processes each arity
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
  "Returns a partition set and entry-point info, suitable for constructing an entry-point fn
  where inputs:
  m - metadata
  address - address of the signature, usually something like #<address foo.0>
  sig - (args, ... code)

  [partition-set, entry-point-info]

  where
  entry-point-info is a hash of:
  :args - arguments extracted from the sig
  :bindings-expression - an expression which constructs a hash containing the arguments

  This can be used to construct an entry-point function:
  (fn [~@args] (flow/exec ~address ~bindings-expression))"
  [m address sig]
  (let [[args & code] sig
        params (params-from-args args [] (-> m :line))
        [start-body, pset, _] (p/partition-body (vec code) address address params)
        pset (pset/add pset address params start-body)
        entry-continuation-bindings (bindings-expr-from-params params)]
    [pset, {:args (vec args)
            :bindings-expression entry-continuation-bindings}])

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
