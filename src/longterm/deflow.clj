(ns longterm.deflow
  (:require [longterm.stack :as stack]
            [longterm.address :as address]
            [longterm.run-store :as rs]
            [longterm.partition :as p]
            [longterm.continuation_set :as cset])
  (:import (longterm.flow Flow)))

(declare params-from-args params-to-continuation-args)

(defmacro deflow
  "Define a long term flow"
  [name docstring? args & code]
  (if-not (string? docstring?)
    `(deflow ~name "" ~docstring? args ~@code)
    (let [params        (params-from-args args)
          address       (address/create name)
          entry-address (address/child address 0)
          [start-body, cset, _] (p/partition-body code address params)
          cset          (cset/add cset entry-address params start-body)
          c-args        (params-to-continuation-args params)]
      `(let [cset#        ~cset         ; compiles the fndefs in the cset
             entry-point# (fn [~@args] ((get cset# entry-address) ~@c-args))]
         (letfn [(~(symbol "wait-for") [event-id# expiry#]
                     (rs/save event-id# expiry#)
                   stack/SUSPEND)]
           (def ~name ~docstring?
             (Flow. '~name entry-point# cset#)))))))

;;
;; HELPERS
;;
(defn- params-to-continuation-args
  "Takes a vector of symbols [p1 p2 p3...] and returns [:p1 p1 :p2 p2 :p3 p3 ...]"
  [params]
  (flatten (map #([(keyword %), %]) params)))

(defn- params-from-args
  "given an argument vector, returns a vector of symbols"
  ([args] (params-from-args args []))
  ([args params]
   (let [arg (first args)]
     (cond
       (= arg '&) (recur (rest args) params)
       (map? arg) (recur (rest args) (concat params (:keys args)))
       (= arg nil) params
       :else (throw (str "Unexpected argument " arg))))))
