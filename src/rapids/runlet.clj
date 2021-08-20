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
  ([field]
   (field (current-run)))
  ([]
   (storage/get-object Run *current-run-id*)))

;;;
;;; Predicates
;;;

; remove for prefector
;(defn suspended? []
;  (r/run-in-state? (current-run) :suspended))

;;
;; run modifiers
;;
(defn update-run! [& kv]
  (swap! @(current-run) ))
(defn initialize-run []
  (update-run! #(assoc % :suspend nil, :response [])))

(defn push-stack! [address bindings data-key]
  {:post [(r/run? %)
          (linked-list? (:stack %))]}
  (let [frame (sf/make-stack-frame address bindings data-key)]
    (update-run! #(assoc % :stack (cons frame (:stack %))))))

(defn pop-stack! []
  (let [popped-frame (atom nil)]
    (update-run! #(let [[frame & rest-stack] (:stack %)]
                    (reset! popped-frame frame)
                    (assoc % :stack (or rest-stack ()))))
    @popped-frame))

(defn add-responses! [& responses]
  (letfn [(push-responses [field]
            #(let [current-response (field %)]
               (assert (vector? current-response))
               (assoc % field (into [] (concat current-response responses)))))]
    (update-run! (push-responses :response))
    responses))

(defn set-result! [result]
  (update-run! #(assoc % :state :complete, :result result)))

(defn set-suspend! [suspend]
  (update-run! #(assoc % :state :suspended, :suspend suspend))
  suspend)

(defn set-listen! [permit expires default]
  "Sets the current run in suspend! state with the given permit, expiry and default value"
  (set-suspend! (s/make-suspend-signal permit expires default)))

