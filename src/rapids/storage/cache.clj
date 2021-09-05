;;
;; This module provides a transactional cache, which:
;;
;; 1. establishes an in-memory cache of objects
;; 2. uses the current connection to retrieve objects from disk into the cache (always locking them)
;; 3. creates and updates objects in the cache before writing to persistent storage
;; 4. writes objects all-or-nothing to the cache within a try/catch block
;;
;; Note: cache-based operations are get-object!, update-object!, create-object!
;;       whereas persistent storage-based operations are get-records!, create-records!, find-records!,
;;
;;       Typically, with-transaction
;;
(ns rapids.storage.cache
  (:require [rapids.storage.connection :as c :refer [*cache*]]
            [rapids.storage.protocol :as p]
            [rapids.util :refer :all :as util]))

(defn cache-exists? [] (boolean *cache*))

(defn save-cache! []
  (doseq [[cls entries] *cache*]
    (let [processed (map (fn [[id {object :object cache-change :op locked? :locked}]]
                           [[id {:object object :locked locked?}],
                            (if (= cache-change :create) object)
                            (if (= cache-change :update) object)])
                      entries)
          synced-entries (apply hash-map (map first processed))
          creates (map second processed)
          updates (map #(nth % 2) processed)]
      (if-not (-> creates count zero?)
        (c/create-records! creates))
      (if-not (-> updates count zero?)
        (c/update-records! updates))
      (setf! *cache* assoc cls synced-entries))))

(declare get-cache-entry set-cache-entry find-in-cache)

(defn cache-get! [cls id]
  "Gets an object from storage, locking it and saving it in cache, of a particular type or loads it from the storage"
  {:pre [(cache-exists?)]}
  (or (get-cache-entry cls id)
    (if-let [obj (first (c/get-records! type id true))]
      (do (set-cache-entry obj :locked true) obj)
      (throw (ex-info (str "Object not found.") {:class cls :id id})))))

(defn cache-update!
  "Updates inst in cache, setting the :op state appropriately: :created objects stay in :created state,
  changed objects are put in :update state."
  [inst]
  {:pre [(cache-exists?)]}
  (let [id (:id inst)
        cls (class inst)
        {existing :object existing-change :op locked :locked} (get-cache-entry cls id)
        cache-change (if existing
                       (or existing-change :update))]
    (assert (or (= :created existing-change) locked)
      (str "Attempt to update unlocked object " (.getName cls) " " id))
    (set-cache-entry inst cache-change)))

(defn cache-create!
  "Adds inst to cache"
  [inst]
  {:pre [(cache-exists?)]}
  (let [id (:id inst)
        cls (class inst)]
    (assert (not (get-cache-entry cls id)) (str "Attempt to create object in cache which already exists: " (.getName cls) id))
    (set-cache-entry inst :create)))

(defn cache-find!
  "Finds objects matching criteria on a single field, loading them from storage as necessary."
  [type field & {:keys [eq lt gt lte gte eq in] :as keys}]
  (let [tests (dissoc keys :limit)
        existing (find-in-cache type field tests)
        excluded-ids (filter :id existing)
        new-objects (apply c/find-records! type field :exclude excluded-ids keys)]
    (map set-cache-entry new-objects)
    (concat existing new-objects)))

(defmacro with-cache
  "Executes body within a transactional cache generating a new connection for that transaction within the storage.
  Note: operations on the storage are performed as a final step."
  [& body]
  `(letfn [(exec# [] ~@body)]
     (c/ensure-connection
       (if (cache-exists?)
         (exec#)
         (binding [*cache* {}]
           (try
             (p/transaction-begin! c/*connection*)
             (exec#)
             (save-cache!)
             (p/transaction-commit! c/*connection*)
             (catch Exception e#
               (p/transaction-rollback! c/*connection*)
               (throw e#))))))))

;;
;; Private Helpers
;;
(defn- matches? [val {:keys [eq lt gt lte gte in] :as tests}]
  (let [ops {:eq =, :lt <, :gt >, :lte <=, :gte >=, :in #(in? %2 %1)}]
    (loop [[[key constraint] & remaining-tests] tests]
      (let [test (key ops)]
        (if (test val constraint)
          (if remaining-tests
            (recur remaining-tests)
            true)
          false)))))

(defn- find-in-cache [type field & {:keys [eq lt gt lte gte in] :as tests}]
  (filter #(matches? (field %1) tests)
    (map :object (get *cache* type))))

(defn- get-cache-entry [cls id]
  (get *cache* [cls id]))

(defn- set-cache-entry
  ([inst] (set-cache-entry inst nil))
  ([inst op]
   (let [cls (class inst)
         id (:id inst)]
     (setf! *cache* update-in [cls id]
       {:object inst
        :op     op}))))
