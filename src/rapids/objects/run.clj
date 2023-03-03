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
   ^Object output
   ^Vector dynamics
   ^UUID interruption-id
   ^PersistentHashMap index])

(def ^:const RunStates #{:running :error :complete :killed})

(defn get-dynamic-values
  "Returns a lazy sequence of bindings, innermost to outermost, for the dynamic var (or symbol representing a dynamic var,
  returning vector if ).

  Usage:
  (get-dynamic-values '*my-dynamic*) ; => returns bindings to *my-dynamic*, innermost first
  (get-dynamic-values #'*my-dynamic*)
  (get-dynamic-values run '*my-dynamic*)
  (get-dynamic-values run #'*my-dynamic*)"
  ([run x]
   {:pre [(or (symbol? x) (var? x))]}
   (if-let [v (if (var? x) x (if (symbol? x) (resolve x)))]
     (->> (:dynamics run)
       reverse
       (filter #(contains? % v))
       (map #(get % v))))))

(defn get-dynamic-value
  "Gets the current binding for symbol or var s

  Usage:
  (get-dynamic-value '*my-dynamic*) ; => returns value of *my-dynamic*
  (get-dynamic-value #'*my-dynamic*)
  (get-dynamic-value run '*my-dynamic*)
  (get-dynamic-value run #'*my-dynamic*)"
  ([run x] (get-dynamic-value run x nil))
  ([run x not-found]
   (let [result (get-dynamic-values run x)]
     (if (empty? result) not-found (first result)))))

(defn valid-run-data? [kvs]
  (every? (fn [key]
            (let [pred ({:id              uuid?
                         :state           RunStates
                         :stack           seq?
                         :index           map?
                         :dynamics        vector?
                         :interrupt       (some-fn nil? uuid?)
                         :waits           (every-pred map? #(every? (fn [[k _]] (uuid? k)) %))
                         :error-info      (some-fn nil? map?)
                         :error-message   (some-fn nil? string?)
                         :output          (constantly true)
                         :result          (constantly true)
                         :interruption-id (some-fn nil? uuid?)
                         :suspend         (some-fn nil? signals/suspend-signal?)} key)
                  val  (get kvs key)]
              (if pred
                (if (pred val)
                  true
                  (throw (ex-info "Invalid data for Run key" {:key key :val val})))
                (throw (ex-info "Invalid key for Run" {:key key})))))
    (keys kvs)))

(defn make-run
  ([] (make-run {}))
  ([{:keys [id, stack, state, output, result, dynamics index]
     :or   {id       (new-uuid)
            state    :running
            stack    ()
            output   []
            dynamics []
            index    {}}
     :as   fields}]
   {:pre  [(RunStates state)]
    :post [(s/assert ::run %)]}
   (map->Run (into (or fields {})
               {:id           id,
                :state        state,
                :stack        stack,
                :output       output,
                :result       result
                :dynamics     dynamics
                :cached-state :created
                :index        index}))))

;(defmethod print-method Run
;  [o w]
;  (print-simple
;    (str "#<Run " (:id o) " " (:start-form o) ">")
;    w))
