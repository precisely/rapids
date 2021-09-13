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
  (:require [rapids.storage.dynamics :refer [ensure-connection *cache*]]
            [rapids.storage.connection-wrapper :as c]
            [rapids.util :refer :all]))

(defn cache-exists? [] (boolean *cache*))

(defn save-cache! []
  (doseq [[cls entries] *cache*]
    (let [synced-entries (reduce-kv #(assoc %1 %2 (dissoc %3 :op)) {} entries)
          cache-entries (vals entries)
          filter-on (fn [op] (map :object (filter #(-> % :op (= op)) cache-entries)))
          creates (filter-on :create)
          updates (filter-on :update)]
      (if (-> creates count (> 0))
        (c/create-records! creates))
      (if (-> updates count (> 0))
        (c/update-records! updates))
      (setf! *cache* assoc cls synced-entries))))

(declare get-cache-entry set-cache-entry find-in-cache)

(defn cache-get! [cls id]
  "Gets an object from storage, locking it and saving it in cache, of a particular type or loads it from the storage"
  {:pre [(cache-exists?)
         (instance? cls Class)
         (not (nil? id))]}
  (or (:object (get-cache-entry cls id))
    (if-let [obj (c/get-record! cls id)]
      (do (set-cache-entry obj) obj)
      (throw (ex-info "Object not found." {:class cls :id id})))))

(defn cache-update!
  "Updates inst in cache, setting the :op state appropriately: :created objects stay in :created state,
  changed objects are put in :update state."
  [inst]
  {:pre [(cache-exists?)]}
  (let [id (:id inst)
        cls (class inst)
        {existing :object existing-change :op} (get-cache-entry cls id)
        cache-change (if existing
                       (or existing-change :update)
                       (throw (ex-info "Attempt to update cache object doesn't exist"
                                {:object inst})))]
    (set-cache-entry inst cache-change)
    inst))

(defn cache-create!
  "Adds inst to cache"
  [inst]
  {:pre [(cache-exists?)]}
  (let [id (:id inst)
        cls (class inst)]
    (assert (not (get-cache-entry cls id)) (str "Attempt to create object in cache which already exists: " (.getName cls) id))
    (set-cache-entry inst :create))
  inst)

(defn cache-find!
  "Finds objects matching criteria on a single field, loading them from storage as necessary."
  [type field & {:keys [eq lt gt lte gte eq in limit order] :as keys}]
  (let [tests (dissoc keys :limit)
        existing (find-in-cache type field tests)
        excluded-ids (filter :id existing)
        test-args (seq (apply concat (map vec tests)))
        new-objects (apply c/find-records! type field :exclude excluded-ids test-args)
        result (concat existing new-objects)
        ordered-result (case order
                         :ascending (sort-by field < result)
                         :descending (sort-by field > result)
                         nil result
                         (throw (ex-info "cache-find! :order must be :ascending :descending or nil"
                                  {:order order})))
        limited-result (if limit (take limit ordered-result) ordered-result)]
    (map set-cache-entry new-objects)
    limited-result))

(defmacro ensure-cached-connection
  "Ensures a transactional cache and connection exists then executes body in the context
  of a transaction. Changes to the cache are committed as a final step or rolled back if
  an exception is detected."
  [& body]
  `(letfn [(exec# [] ~@body)]
     (ensure-connection
       (if (cache-exists?)
         (exec#)
         (binding [*cache* {}]
           (try
             (c/transaction-begin!)
             (let [result# (exec#)]
               (save-cache!)
               (c/transaction-commit!)
               result#)
             (catch Exception e#
               (c/transaction-rollback!)
               (throw e#))))))))

;;
;; Private Helpers
;;
(defn- matches? [val {:keys [eq lt gt lte gte in] :as tests}]
  (let [ops {:eq =, :lt <, :gt >, :lte <=, :gte >=, :in #(in? %2 %1)}]
    (loop [[[key constraint] & remaining-tests] (select-keys tests [:eq :lt :gt :lte :gte :in])]
      (let [test (key ops)]
        (if (test val constraint)
          (if remaining-tests
            (recur remaining-tests)
            true)
          false)))))

(defn- find-in-cache [cls field {:keys [eq lt gt lte gte in] :as tests}]
  (filter #(matches? (field %1) tests) (map :object (vals (get *cache* cls)))))

(defn get-cache-entry [cls id]
  (get-in *cache* [cls id]))

(defn set-cache-entry
  ([inst] (set-cache-entry inst nil))
  ([inst op]
   {:pre  [(not (nil? inst))]
    :post [(get-in *cache* [(class inst) (:id inst)])]}
   (let [cls (class inst)
         id (:id inst)]
     (setf! *cache* update-in [cls id]
       (constantly {:object inst
                    :op     op})))))
