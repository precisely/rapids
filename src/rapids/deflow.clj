(ns rapids.deflow
  (:require [rapids.address :as address]
            rapids.run-loop
            [rapids.util :refer [qualify-symbol]]
            [rapids.partition :as p]
            [rapids.partition-utils :refer [bindings-expr-from-params]]
            [rapids.partition-set :as pset])
  (:import (rapids.flow Flow)))

(declare params-from-args params-to-continuation-args expand-flow)

(defmacro deflow
  "Define a long term flow which suspends execution at (suspend ...) expressions.
  Flows are started with runloop/start! and resumed "
  [name docstring? args & code]
  (if-not (string? docstring?)
    (with-meta `(deflow ~name "" ~docstring? ~args ~@code) (meta &form))
    (expand-flow name docstring? args code)))

(defn expand-flow [name docstring? args code]
  (let [params (params-from-args args)
        qualified (qualify-symbol name)
        address (address/create qualified)
        [start-body, pset, _] (p/partition-body (vec code) address address params)
        pset (pset/add pset address params start-body)
        entry-continuation-bindings (bindings-expr-from-params params)
        entry-point-name (symbol (str name "__entry-point"))]
    `(let [cset# ~(pset/continuation-set-def pset)          ; compiles the fndefs in the pset
           entry-continuation# (get cset# ~address)
           entry-point# (fn ~entry-point-name [~@args] (entry-continuation# ~entry-continuation-bindings))]
       (def ~name ~docstring?
         (Flow. '~qualified, entry-point#, cset#, ~pset)))))

;;
;; HELPERS
;;
(defn- params-to-continuation-args
  "Takes a vector of symbols [p1 p2 p3...] and returns [:p1 p1 :p2 p2 :p3 p3 ...]"
  [params]
  (flatten (map (fn [p]
                  [(keyword p), p])
                params)))

(defn- params-from-args
  "given an argument vector, returns a vector of symbols"
  ([args] (params-from-args args []))
  ([args params]
   (let [arg (first args)]
     (cond
       (= arg '&) (recur (rest args) params)
       (map? arg) (recur (rest args) (concat params (:keys arg)))
       (symbol? arg) (recur (rest args) (conj params arg))
       (nil? arg) (vec params)
       :else (throw (ex-info (str "Unexpected argument " arg)
                      {:type :compiler-error}))))))
