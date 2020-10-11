(ns longterm.deflow
  (:require [longterm.address :as address]
            longterm.runloop
            [longterm.util :refer [qualify-symbol]]
            [longterm.partition :as p]
            [longterm.partition-set :as pset])
  (:use clojure.tools.trace)
  (:import (longterm.flow Flow)))

(declare params-from-args params-to-continuation-args)

(defmacro deflow
  "Define a long term flow which suspends execution at (suspend ...) expressions.
  Flows are started with runner/start-flow! and resumed "
  [name docstring? args & code]
  (if-not (string? docstring?)
    `(deflow ~name "" ~docstring? ~args ~@code)
    (let [params  (params-from-args args)
          qualified (qualify-symbol name)
          address (address/create qualified)
          [start-body, pset, suspend?] (p/partition-body (vec code) address address params)
          pset    (pset/add pset address params start-body)
          c-args  (params-to-continuation-args params)
          entry-point-name (symbol (str name "__entry-point"))]
      (if-not suspend?
        (throw (Exception. (format "Flow %s doesn't suspend. Consider using defn instead." name))))
      `(let [cset#               ~(pset/continuation-set-def pset) ; compiles the fndefs in the pset
             entry-continuation# (get cset# ~address)
             entry-point#        (fn ~entry-point-name [~@args] (entry-continuation# ~@c-args))]
         (def ~name ~docstring?
           (Flow. '~qualified, entry-point#, cset#, ~pset))))))

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
       :else (throw (Exception. (str "Unexpected argument " arg)))))))
