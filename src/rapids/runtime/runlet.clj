;;
;; The runlet context is a set of dynamic bindings representing:
;; 1. the current run, which may differ from the root run as a result of redirection or return from redirection
;; 2. other runs which may have been created or modified during the request
;; 3. pools created or used during the runlet
;;
(ns rapids.runtime.runlet
  (:require rapids.objects.run
            [rapids.objects.stack-frame :as sf]
            [rapids.objects.signals :refer [suspend-signal? make-suspend-signal]]
            [rapids.storage.core :as s]
            [rapids.support.util :refer :all])
  (:import (rapids.objects.run Run)))

;;;
;;; Current Run
;;;
(def ^{:dynamic true
       :doc     "The id of the currewnt run"}
  *current-run-id*)

(defn run? [o]
  (and (s/cache-proxy? o)
    (= Run (.theClass o))))

(defmacro with-run
  "Ensures a run is in the cache and establishes it as the current run. Used by internal functions."
  [run-form & body]
  `(let [run# ~run-form]
     (assert (run? run#))
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
   (s/->CacheProxy Run *current-run-id*)))

;;
;; Operations
;;

(defn update-run!
  "Updates the current run, returning the new run"
  [& kvs]
  (.update (current-run) #(apply assoc % kvs)))

(defn initialize-run-for-runlet []
  (update-run! :suspend nil, :response []))

(defn push-stack! [address bindings data-key]
  {:post [(run? %)
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
  {:pre [(= (current-run :state) :running)
         (not= *current-run-id* (:id child-run))]}
  (.setKey child-run :parent-run-id *current-run-id*))

(defn complete-run! [result]
  {:pre [(= (current-run :state) :running)
         (not (suspend-signal? result))]}
  (update-run! :state :complete, :result result))

(defn suspend-run! [suspend]
  {:pre [(= (current-run :state) :running)
         (suspend-signal? suspend)]}
  (update-run! :suspend suspend))
