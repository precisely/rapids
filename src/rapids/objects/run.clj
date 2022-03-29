;;
;; Describes the Run data structure
;;
(ns rapids.objects.run
  (:require
    [clojure.spec.alpha :as s]
    [rapids.objects.signals :as signals]
    [rapids.support.util :refer :all])
  (:import (clojure.lang Cons Keyword PersistentHashMap Symbol Var)
           (java.util UUID Vector)))

(defrecord Run
  [^UUID id
   ^Keyword state
   ^Cons stack
   ^Object result
   ^Object response
   ^Vector dynamics
   ^UUID interruption-id
   ^PersistentHashMap index])

(def ^:const RunStates #{:running :error :complete})

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
                         :index        map?
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
  ([{:keys [id, stack, state, response, result, dynamics index]
     :or   {id       (new-uuid)
            state    :running
            stack    ()
            response []
            dynamics []
            index   {}}
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
                    :index       index}))))
