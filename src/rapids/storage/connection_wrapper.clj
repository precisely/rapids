;;
;; Provides StorageConnection protocol methods for the current connection
;;
(ns rapids.storage.connection-wrapper
  (:require
    [rapids.storage.protocol :as p]
    [rapids.storage.globals :refer :all]))

(declare instances-of? instances-of-same?)
(defn transaction-begin! []
  (p/transaction-begin! *connection*))

(defn transaction-commit! []
  (p/transaction-commit! *connection*))

(defn transaction-rollback! []
  (p/transaction-rollback! *connection*))

(defn create-records!
  [objects]
  {:post [(instances-of-same? %)]}
  (p/create-records! *connection* objects))

(defn create-record! [o]
  (create-records! [o]))

(defn update-records!
  [objs]
  {:pre  [(current-connection)]
   :post [(instances-of-same? %)]}
  (p/update-records! *connection* objs))

(defn update-record! [o]
  (update-records! [o]))

(defn get-records!
  [cls ids]
  {:pre [*connection*]}
  (p/get-records! *connection* cls ids))

(defn get-record! [cls id]
  (first (get-records! cls [id])))

(defn find-records!
  [type field & {:keys [eq lt gt lte gte eq limit in exclude] :as keys}]
  {:post [(instances-of? type %)]}
  (p/find-records! *connection* type field keys))

(defn find-record!
  [type field & {:keys [eq lt gt lte gte eq limit in exclude] :as keys}]
  (first (apply find-records! type field keys)))

;; HELPERS
(defn- instances-of? [cls seq]
  (every? #(instance? cls %) seq))

(defn- instances-of-same? [seq]
  (instances-of? (class (first seq)) seq))