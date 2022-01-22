;;
;; Describes the Run data structure
;;
(ns rapids.objects.run
  (:require
    [clojure.spec.alpha :as s]
    [clojure.tools.macro :refer [macrolet]]
    [java-time :as t]
    [rapids.objects.signals :as signals]
    [rapids.objects.address :as a]
    [rapids.objects.stack-frame :as sf]
    [rapids.support.util :refer :all])
  (:import (java.util UUID Vector)
           (clojure.lang Keyword Cons Symbol Var PersistentHashMap)))

(defrecord Run
  [^UUID id
   ^Keyword state
   ^Cons stack
   ^Object result
   ^Object response
   ^Vector dynamics
   ^UUID interruption-id
   ^PersistentHashMap status])

(def ^:const RunStates #{:running :error :complete :interrupted})

(defn get-dynamic-values
  "Returns a lazy sequence of bindings, innermost to outermost, for the dynamic var (or symbol representing a dynamic var,
  returning not-found or nil)."
  ([run x] (get-dynamic-values run x nil))
  ([run x not-found]
   {:pre [(symbol? x) (var? x)]}
   (if-let [v (case (type x)
                Var x
                Symbol (resolve x))]
     (->> (:dynamics run)
          reverse
          (filter #(contains? % v))
          (map #(get % v not-found))))))

(defn get-dynamic-value
  "Gets the current binding for symbol or var s"
  ([run x] (get-dynamic-value run x nil))
  ([run x not-found]
   (first (get-dynamic-values run x not-found))))

(defn valid-run-data? [kvs]
  (every? (fn [key]
            (let [pred ({:id            uuid?
                         :state         RunStates
                         :stack         seq?
                         :status        map?
                         :dynamics      vector?
                         :interrupt     (some-fn nil? uuid?)
                         :parent-id     (some-fn nil? uuid?)
                         :error-info    (some-fn nil? map?)
                         :error-message (some-fn nil? string?)
                         :output      (constantly true)
                         :result        (constantly true)
                         :suspend       (some-fn nil? signals/suspend-signal?)} key)
                  val  (get kvs key)]
              (if pred
                (pred val)
                (throw (ex-info "Invalid key for Run" {:key key})))))
          (keys kvs)))

(defn make-run
  ([] (make-run {}))
  ([{:keys [id, stack, state, response, result, dynamics status]
     :or   {id       (new-uuid)
            state    :running
            stack    ()
            response []
            dynamics []
            status   {}}
     :as   fields}]
   {:pre  [(RunStates state)]
    :post [(s/assert ::run %)]}
   (map->Run (into (or fields {})
                   {:id           id,
                    :state        state,
                    :stack        stack,
                    :output     response,
                    :result       result
                    :dynamics     dynamics
                    :cached-state :created
                    :status       status}))))

;(defmethod print-method Run
;  [o w]
;  (print-simple
;    (str "#<Run " (:id o) " " (:start-form o) ">")
;    w))
