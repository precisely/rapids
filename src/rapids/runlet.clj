;;
;; The runlet context is a set of dynamic bindings representing:
;; 1. the current run, which may differ from the root run as a result of redirection or return from redirection
;; 2. other runs which may have been created or modified during the request
;; 3. pools created or used during the runlet
;;
(ns rapids.runlet
  (:require [rapids.util :refer :all]
            [rapids.storage :as storage]
            [rapids.stack-frame :as sf]
            [rapids.signals :refer [make-suspend-signal]]
            [rapids.signals :as s]
            [rapids.run :as r])
  (:import (rapids.run Run)))

;;;
;;; Current Run
;;;
(def ^{:dynamic true
       :doc     "The id of the current run"}
  *current-run-id*)

(defmacro with-run
  "Ensures a run is in the cache and establishes it as the current run. Used by internal functions."
  [run-form & body]
  `(let [run# ~run-form]
     (assert (r/run? run#))
     (binding [*current-run-id* (:id run#)]
       ~@body)))

(defn current-run
  "Returns the current run, or optionally returns a field of the current run.

  (current-run) => the current run
  (current-run :id) => id of the current run
  etc"
  ([& fields]
   (get-in (current-run) fields))
  ([]
   (storage/cache-get! Run *current-run-id*)))

;;
;; Operations
;;

(defn update-run!
  "Updates the current run, returning the new run"
  [& kvs]
  (storage/cache-update! (apply assoc (current-run) kvs)))

(defn initialize-run-for-runlet []
  (update-run! :suspend nil, :response []))

(defn push-stack! [address bindings data-key]
  {:post [(r/run? %)
          (linked-list? (:stack %))]}
  (let [frame (sf/make-stack-frame address bindings data-key)]
    (update-run! :stack (cons frame (current-run :stack)))))

(defn pop-stack! []
  (let [[frame & rest-stack] (current-run :stack)]
    (update-run! :stack (or rest-stack ()))
    frame))

(defn add-responses! [& responses]
  (let [current-response (current-run :response)]
    (if (not (vector? current-response))
      (println "Ooops - current-response = " current-response))
    (assert (vector? current-response))
    (update-run! :response (vec (concat current-response responses)))
    responses))

(defn attach-child-run!
  [child-run]
  {:pre [(r/run-in-state? (current-run) :running)
         (r/run-in-state? child-run :running)
         (not= *current-run-id* (:id child-run))]}
  (storage/cache-update! (assoc child-run :parent-run-id *current-run-id*)))

(defn complete-run! [result]
  {:pre [(r/run-in-state? (current-run) :running)
         (not (s/suspend-signal? result))]}
  (update-run! :state :complete, :result result))

(defn suspend-run! [suspend]
  {:pre [(r/run-in-state? (current-run) :running)
         (s/suspend-signal? suspend)]}
  (update-run! :suspend suspend))
