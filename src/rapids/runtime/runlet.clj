;;
;; The runlet context is a set of dynamic bindings representing:
;; 1. the current run, which may differ from the root run as a result of redirection or return from redirection
;; 2. other runs which may have been created or modified during the request
;; 3. pools created or used during the runlet
;;
(ns rapids.runtime.runlet
  (:require [rapids.objects.run :as r]
            [rapids.runtime.globals :refer :all]
            [rapids.objects.stack-frame :as sf]
            [rapids.objects.signals :refer [suspend-signal? make-suspend-signal]]
            [rapids.storage.core :as s]
            [rapids.support.util :refer :all])
  (:import (rapids.objects.run Run)))

(defn run? [o]
  (and (s/cache-proxy? o)
       (= (.getName Run) (-> o (.theClass) (.getName)))))

(defn resolve-run [o]
  (let [run-id (if (run? o) (:id o) o)
        run (s/cache-get! Run run-id)]
    (assert (run? run) ("Unable to locate Run" o))
    run))

(defmacro with-run
  "Ensures a run is in the cache and establishes it as the current run. Used by internal functions."
  [run-or-id & body]
  `(binding [*current-run-id* (:id (resolve-run ~run-or-id))]
     ~@body))


(defn current-run
  "Returns the current run, or optionally returns a field of the current run.

  (current-run) => the current run
  (current-run :id) => id of the current run
  etc"
  ([& fields]
   (get-in (current-run) fields))
  ([]
   (s/cache-get! Run *current-run-id*)))

;;
;; Operations
;;
(defn update-run!
  "Updates the current run, returning the new run"
  [& {:keys [] :as kvs}]
  {:pre [(r/valid-run-data? kvs)]}
  (.update (current-run) #(merge % kvs)))

(defn initialize-run-for-runlet []
  (update-run! :suspend nil, :output [], :error-message nil, :error-info nil))

(defn push-stack! [address bindings input-key]
  {:post [(run? %)
          (linked-list? (:stack %))]}
  (let [frame (sf/make-stack-frame address bindings input-key)]
    (update-run! :stack (cons frame (current-run :stack)))))

(defn pop-stack! []
  (let [[frame & rest-stack] (current-run :stack)]
    (update-run! :stack (or rest-stack ()))
    frame))

(defn ^{:arglists '[[run & kvs] [& kvs]]}
  set-index!
  "Sets a index key (or subkey) of the given run (or the current run).

  E.g.,
  (set-index! run :a 1) => {:a 1}
  (set-index! :a 1) => {:a 1}
  (set-index! :a 1 :b 2) ; {:a 1, :b 2}
  (set-index! [:a :b] 2) ; {:a {:b 2}}"
  [& kvs]
  (let [[maybe-run & maybe-kvs] kvs]
    (if (run? maybe-run)
      (with-run maybe-run
        (apply set-index! maybe-kvs))
      (update-run! :index (reduce (fn [m [k v]]
                                     (let [ks (if (vector? k) k [k])]
                                       (assoc-in m ks v)))
                                   (current-run :index)
                                   (partition 2 kvs))))))

(defn add-responses! [& responses]
  (let [current-response (current-run :output)]
    (assert (vector? current-response))
    (update-run! :output (vec (concat current-response responses)))
    responses))

(defn interrupt-run!
  "Low level function which sets the run in interrupted state and returns the interruption id"
  []
  (let [uuid (new-uuid)]
    (assert (= :running (current-run :state)) "Attempt to interrupt run which is not in :running state")
    (assert (nil? (current-run :interrupt)) "Attempt to interrupt run which is already interrupted")
    (update-run! :interrupt uuid)
    uuid))

;; code inspector may show this as unused, but it is referenced by code generated by the partitioner
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

(defn update-dynamics! [dynamics]
  {:pre [(vector dynamics)]}
  (update-run! :dynamics dynamics))

;;
;; Dynamic bindings
;;
(defn push-run-bindings!
  [bindings]
  {:pre [(map? bindings)]}
  (update-dynamics! (conj (current-run :dynamics) bindings)))

(defn pop-run-bindings! []
  (update-dynamics! (pop (current-run :dynamics))))

(defn set-run-dynamic-var
  "Traverses current run bindings, most recent first (:dynamics is a vec which grows at the tail),
  changing the association in the first map which contains lhs."
  [lhs rhs]
  {:pre [(var? lhs)]}
  (letfn [(assoc-dynamics [dynvar val]
            (loop [[bindings & dynamics] (reverse (current-run :dynamics))
                   new-dynamics []]                         ; concat
              (if (contains? bindings dynvar)
                (vec (doall (reverse (concat (conj new-dynamics (assoc bindings dynvar val)) dynamics))))
                (if (empty? dynamics)
                  (throw (ex-info "Attempt to set! run dynamic which has not been bound"
                                  {:var dynvar :value val}))
                  (recur dynamics (conj new-dynamics bindings))))))]
    (update-run! :dynamics (assoc-dynamics lhs rhs))))

(defn enter-binding-body [f, bindings, _]
  (push-thread-bindings bindings)
  (try (f)
       (catch Exception e#
         (pop-run-bindings!)
         (throw e#))
       (finally (pop-thread-bindings))))
