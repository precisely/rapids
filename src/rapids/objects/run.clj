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
           (clojure.lang Keyword Cons Symbol Var)))

(defrecord Run
  [^UUID id
   ^Keyword state
   ^Cons stack
   ^Object result
   ^Object response
   ^Vector dynamics
   ^UUID interruption-id])

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
            (let [pred ({:id           uuid?,
                         :state        RunStates,
                         :stack        seq?
                         :dynamics     vector?
                         :interrupt (some-fn nil? uuid?)
                         :parent-id    (some-fn nil? uuid?)
                         :response     (constantly true)
                         :result       (constantly true)
                         :suspend      (some-fn nil? signals/suspend-signal?)} key)
                  val (get kvs key)]
              (if pred
                (pred val)
                (throw (ex-info "Invalid key for Run" {:key key})))))
    (keys kvs)))

(defn make-run
  ([] (make-run {}))
  ([{:keys [id, stack, state, response, result dynamics]
     :or   {id       (new-uuid)
            state    :running
            stack    ()
            response []
            dynamics []}
     :as   fields}]
   {:pre  [(RunStates state)]
    :post [(s/assert ::run %)]}
   (map->Run (into (or fields {})
               {:id           id,
                :state        state,
                :stack        stack,
                :response     response,
                :result       result
                :dynamics     dynamics
                :cached-state :created}))))

(defn make-test-run
  "Returns a run and a record"
  [& remove-keys]
  {:post [(not (contains-some? % remove-keys))]}
  (make-run                                                 ; fill every field of the run
    (apply dissoc {:state         :running
                   :start-form    (str '(foo :a 1))
                   :stack         (list (sf/make-stack-frame (a/->address 'foo 1 2) {:b 2} 'data-key))
                   :suspend       (signals/make-suspend-signal :foo (t/local-date-time) {:a 1})
                   :response      [:hello :there]
                   :result        {:data "some-result"}
                   :parent-run-id (new-uuid)
                   :dynamics      [{#'*print-dup* true}]
                   :error         (Exception. "foo")}
      remove-keys)))

(defmethod print-method Run
  [o w]
  (print-simple
    (str "#<Run " (:id o) " " (:start-form o) ">")
    w))
