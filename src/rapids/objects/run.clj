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
           (clojure.lang Keyword)))

(defrecord Run
  [^UUID id
   ^Keyword state
   ^Vector stack
   ^Object result
   ^Object response])

(defn raw-run? [run] (instance? Run run))

(def ^:const RunStates #{:running :error :complete})

(defn make-run
  ([] (make-run {}))

  ([{:keys [id, stack, state, response, result]
     :or   {id       (UUID/randomUUID)
            state    :running
            stack    ()
            response []}
     :as   fields}]
   {:post [(s/assert ::run %)]}
   (map->Run (into (or fields {})
               {:id           id,
                :state        state,
                :stack        stack,
                :response     response,
                :result       result
                :cached-state :created}))))

(defn make-test-run
  "Returns a run and a record"
  [& remove-keys]
  {:post [(not (contains-some? % remove-keys))]}
  (let [run (make-run                                       ; fill every field of the run
              (apply dissoc {:state         :running
                             :start-form    (str `(foo :a 1))
                             :stack         (list (sf/make-stack-frame (a/->address `foo 1 2) {:b 2} 'data-key))
                             :suspend       (signals/make-suspend-signal :foo (t/local-date-time) {:a 1})
                             :response      [:hello :there]
                             :result        {:data "some-result"}
                             :parent-run-id (UUID/randomUUID)
                             :error         (Exception. "foo")}
                remove-keys))]
    ;record (run-to-record run)]
    run))

(defmethod print-method Run
  [o w]
  (print-simple
    (str "#<Run " (:id o) " " (:start-form o) ">")
    w))
