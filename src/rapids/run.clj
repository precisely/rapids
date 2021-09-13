;;
;; Describes the Run data structure
;;
(ns rapids.run
  (:require
    [rapids.util :refer :all]
    [rapids.signals :as signals]
    [rapids.address :as a]
    [clojure.spec.alpha :as s]
    [rapids.stack-frame :as sf]
    [java-time :as t]
    [clojure.tools.macro :refer [macrolet]])
  (:import (java.util UUID)))

(declare run-in-state?)

(defrecord
  Run [id state stack result run-response response])

(defn run? [run] (instance? Run run))

(def ^:const RunStates #{:running :error :complete})
(defn run-in-state?
  [run & states]
  {:pre [(every? #(in? RunStates %) states)]}
  (let [state (:state run)
        result (and (run? run) (or (in? states state) (in? states :any)))]
    result))

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
                             :stack         (list (sf/make-stack-frame (a/create `foo 1 2) {:b 2} 'data-key))
                             :suspend       (signals/make-suspend-signal :foo (t/local-date-time) {:a 1})
                             :response      [:hello :there]
                             :result        {:data "some-result"}
                             :parent-run-id (UUID/randomUUID)
                             :error         (Exception. "foo")}
                remove-keys))]
    ;record (run-to-record run)]
    run))