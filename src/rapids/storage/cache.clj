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
  (:require [rapids.storage.globals :refer [ensure-connection *cache*]]
            [rapids.storage.connection-wrapper :as c]
            [rapids.storage.in-memory-filter :refer [filter-records]]
            [rapids.support.util :refer :all])
  (:import (rapids.storage CacheProxy)))

(declare ensure-raw-object ->CacheProxy cache-proxy?)

(defn cache-exists? [] (boolean *cache*))

(defn save-cache! []
  (doseq [[cls entries] *cache*]
    (let [synced-entries (reduce-kv #(assoc %1 %2 (dissoc %3 :op)) {} entries)
          cache-entries  (vals entries)
          filter-on      (fn [op] (map :object (filter #(-> % :op (= op)) cache-entries)))
          creates        (filter-on :create)
          updates        (filter-on :update)]
      (if (-> creates count (> 0))
        (c/create-records! creates))
      (if (-> updates count (> 0))
        (c/update-records! updates))
      (setf! *cache* assoc cls synced-entries))))

(declare get-cache-entry set-cache-entry find-in-cache ensure-raw-object)

(defn cache-get!
  "Gets an object from storage, locking it and saving it in cache, of a particular type or loads it from the storage"
  ([cls id & keys]
   (get-in (ensure-raw-object cls id) keys))

  ([cls id]
   {:pre [(cache-exists?)
          (instance? Class cls)
          (not (nil? id))]}
   (if-let [fresh (ensure-raw-object cls id)]
     (->CacheProxy cls id fresh))))

(defn cache-insert!
  "Adds inst to cache, returning a CacheProxy"
  [inst]
  {:pre [(cache-exists?)]}
  (let [id  (:id inst)
        cls (class inst)]
    (assert (not (get-cache-entry cls id)) (str "Attempt to create object in cache which already exists: " (.getName cls) id))
    (set-cache-entry inst :create)
    (->CacheProxy cls id inst)))

(defn cache-find!
  "Finds objects matching criteria on a single field, loading them from storage as necessary.

  Returns - list of CacheProxy objects"
  ([type field-constraints] (cache-find! type field-constraints {}))
  ([type field-constraints query-constraints]
   (let [existing             (filter-records (map :object (vals (get *cache* type)))
                                              field-constraints {})
         excluded-ids         (map :id existing)
         storage-constraints  (if (empty? excluded-ids)
                                field-constraints
                                (conj field-constraints [:id :not-in (vec excluded-ids)]))
         new-objects          (c/find-records! type storage-constraints query-constraints)
         result               (concat existing new-objects)
         filtered-result      (filter-records result [] query-constraints)]
     (map set-cache-entry new-objects)
     (map #(->CacheProxy (class %) (:id %) %) filtered-result))))
;;
;;(defn cache-find!
;;  "Finds objects matching criteria on a single field, loading them from storage as necessary.
;;
;;  Returns - list of CacheProxy objects"
;;  [type field & {:keys [eq lt gt lte gte eq in limit order-by] :as query}]
;;  (let [tests           (dissoc keys :limit)
;;        existing        (filter-records (map :object (vals (get *cache* type)))
;;                                        field (dissoc keys :limit :order-by))
;;        excluded-ids    (filter :id existing)
;;        test-args       (seq (apply concat (map vec tests)))
;;        new-objects     (apply c/find-records! type field :exclude excluded-ids test-args)
;;        result          (concat existing new-objects)
;;        filtered-result (filter-records result field {:limit limit :order-by order-by})]
;;    (map set-cache-entry new-objects)
;;    (map #(->CacheProxy (class %) (:id %) %) filtered-result)))

(defn call-with-cached-transaction [f]
  (binding [*cache* {}]
    (try
      (c/transaction-begin!)
      (let [result (f)]
        (save-cache!)
        (c/transaction-commit!)
        result)
      (catch Exception e
        (c/transaction-rollback!)
        (throw e)))))

(defmacro ensure-cached-connection
  "Ensures a transactional cache and connection exists then executes body in the context
  of a transaction. Changes to the cache are committed as a final step or rolled back if
  an exception is detected."
  [& body]
  `(let [exec# (fn [] ~@body)]
     (ensure-connection
       (if (cache-exists?)
         (exec#)
         (call-with-cached-transaction exec#)))))

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
   (let [cls         (class inst)
         id          (:id inst)
         cache-entry {:object inst
                      :op     op}]
     (setf! *cache* update-in [cls id]
            (constantly cache-entry))
     cache-entry)))

(defn ensure-raw-object
  "Ensures the raw object is in the cache and returns it. If it doesn't exist, throws an error."
  [cls id]
  {:pre [(instance? Class cls)
         (not (nil? id))]}
  (or (:object (get-cache-entry cls id))
      (if-let [obj (c/get-record! cls id)]
        (do (set-cache-entry obj) obj)
        (throw (ex-info "Object not found."
                        {:type  ::not-found
                         :class cls
                         :id    id})))))

(defn ->CacheProxy
  ([cls id] (->CacheProxy cls id nil))
  ([cls id obj]
   (CacheProxy. cls id obj)))

(defn cache-proxy? [o]
  (and o (instance? CacheProxy o)))
