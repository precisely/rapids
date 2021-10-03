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
           (clojure.lang Keyword Cons)))

(defrecord Run
  [^UUID id
   ^Keyword state
   ^Cons stack
   ^Object result
   ^Object response
   ^Vector dynamics])

(def ^:const RunStates #{:running :error :complete})

(defn make-run
  ([] (make-run {}))

  ([{:keys [id, stack, state, response, result dynamics]
     :or   {id       (UUID/randomUUID)
            state    :running
            stack    ()
            response []
            dynamics []}
     :as   fields}]
   {:post [(s/assert ::run %)]}
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
                   :start-form    (str `(foo :a 1))
                   :stack         (list (sf/make-stack-frame (a/->address `foo 1 2) {:b 2} 'data-key))
                   :suspend       (signals/make-suspend-signal :foo (t/local-date-time) {:a 1})
                   :response      [:hello :there]
                   :result        {:data "some-result"}
                   :parent-run-id (UUID/randomUUID)
                   :dynamics      [{#'*print-dup* true}]
                   :error         (Exception. "foo")}
      remove-keys)))

(defmethod print-method Run
  [o w]
  (print-simple
    (str "#<Run " (:id o) " " (:start-form o) ">")
    w))
