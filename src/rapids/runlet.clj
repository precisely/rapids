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
            [rapids.run :as r]))

(def ^{:dynamic true
       :doc     "The id of the current run"}
  *current-run-id*)

(def ^{:dynamic true
       :doc     "A map of object classes to instances"}
  *cache*)

(declare get-object update-object! save-cache! with-runlet with-run)

(defn in-runlet? [] (bound? #'*cache*))

(defmacro with-runlet [[storage] & body]
  "Sets up the dynamic environment for the runlet. This includes establishing
  a cache and a transaction which lasts the duration of the runlet. Objects created
  and stored in the cache are persisted at the end of the transaction.

  This is a user-land API."
  `(letfn [(dobody# []
             (assert (bound? #'*cache*))
             ~@body)]
     (if (in-runlet?)
       (dobody#)
       (binding [*cache* {}]
         (storage/with-transaction [~storage]
           (let [result# (dobody#)]
             (save-cache!)
             result#))))))

(defmacro with-run
  "Ensures a run is in the cache and establishes it as the current run. Used by internal functions."
  [[run-form#] & body]
  `(let [run# (update-object! ~run-form#)]
     (binding [*current-run-id* (:id run#)]
       ~@body)))
;;;
;;; Cache operations
;;;

(defn dirty? [obj] (-> obj :cache-state :dirty))
(defn created? [obj] (-> obj :cache-state :created))
(defn dirty-object [obj] (assoc obj :cache-state :dirty))
(defn created-object [obj] (assoc obj :cache-state :created))

(defn save-cache! []
  (doseq [[_ objects] *cache*]
    (doseq [[_ object] objects]
      (when (:dirty object)
      (update-object! (dissoc object :dirty))
      (storage/save! object)))))

(defn get-object [cls id]
  (get-in *cache* [cls id]))

(defn update-object!
  [inst]
  {:pre [(bound? #'*cache*)]}
  (let [id (:id inst)
        cls (class inst)
        existing (get-object cls (:id inst))]
    (if (not= inst existing)
      (reset! *cache* (update-in *cache* [cls id] (fn [_] (assoc existing :dirty true)))))))

(defn acquire-run!
  "Acquires the run, setting it in :running state, and clearing the suspend object"
  [run-id permit]
  (let [{{s-permit :permit} :suspend :as retrieved} (load-run! run-id)]
    (if-not (r/run-in-state? retrieved :suspended :created)
      (throw (ex-info (str "Cannot acquire Run " run-id " from cache in state " (:state retrieved))
               {:type :runtime-error})))
    (when (not= s-permit permit)
      (throw (ex-info (str "Cannot acquire Run " run-id " invalid permit")
               {:type :input-error})))

    (cache-run! (assoc retrieved :state :running, :response [], :suspend nil))))

(defn load-run!
  "Gets the run from the cache or storage"
  [run-id]
  (or
    (get *run-cache* run-id)
    (ifit [run (storage/lock-run! run-id)]
      (cache-run! run))))

(defn load-pool!
  "Gets a pool from the cache or storage"
  [pool-id]
  (or
    (get *pool-cache* pool-id)
    (ifit [pool (storage/lock-pool! pool-id)]
      (cache-run! pool))))

;;;
;;; Current Run
;;;
(defn current-run
  "Returns the currently active run"
  []
  (load! *current-run-id*))


;;
;; accessors
;;
(defn id [] (:id (current-run)))

(defn stack [] (:stack (current-run)))

(defn response [] (:response (current-run)))

(defn result [] (:result (current-run)))

(defn parent-run-id [] (:parent-run-id (current-run)))

;;;
;;; Predicates
;;;
(defn suspended? []
  (r/run-in-state? (current-run) :suspended))

;;
;; run modifiers
;;
(defn initialize-runlet []
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

;;
;; state change
;;

(defn- update-run!
  "Alters the provided run (defaults to the the current run if no run is provided)"
  ([f] (update-run! (current-run) f))

  ([run f]
   (let [new-run (f run)
         new-run (if (not= run new-run) (assoc new-run :dirty true) new-run)]
     (cache-run! new-run))))
