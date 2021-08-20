(ns rapids.storage.connection
  (:require
    [rapids.storage.protocol :as p]
    [rapids.storage.persistence :refer [freeze thaw extend-freeze extend-thaw]]
    [rapids.util :refer [in? new-uuid]]
    [rapids.run :as r]
    [rapids.util :refer :all]
    [rapids.signals :as s]
    [rapids.storage.protocol :as p]))

(declare set-storage! with-storage!)

(def ^:dynamic *storage* (atom nil))
(def ^:dynamic *connection* nil)

;;
;; Public API based on storage and stack/*stack* globals
;;

(defn set-storage!
  "Sets the storage - useful for setting a top-level storage."
  [storage]
  (reset! *storage* storage))

(defmacro with-storage
  "Override the existing storage. This may be useful if multiple storages are used."
  [[storage] & body]
  `(binding [*storage* (atom ~storage)
             *connection* nil
             *cache* nil]
     ~@body))

(defmacro with-connection
  [& body]
  `(binding [*connection* (p/get-connection *storage*)
             *cache* nil]
     (try ~@body
          (finally (.close *connection*)))))

(defmacro ensure-connection
  "Ensures *connection* is a valid connection"
  [& body]
  `(letfn [(exec# [] ~@body)]
     (if *connection*
       (exec#)
       (with-connection (exec#)))))

(defn create-record!
  [object]
  {:pre [(atom? object)
         (satisfies? p/Storage @*storage*)
         ()]}
  (p/create-record! @*storage* object))

(defn update-record!
  [obj]
  {:pre [(satisfies? p/Storage @*storage*)]}
  (p/update-record! @*storage* obj))

(defn get-record
  [type id]
  {:post [(atom? %) (instance? type @%)]}
  (p/get-record @*storage* type id))

(defn lock-record!
  [type id]
  {:pre  [(not (nil? id))]
   :post [(atom? %) (instance? type @%)]}
  ;;       good hygiene to avoid potential deadlocks; not yet critical.
  (p/lock-record! @*storage* type id))

(defn lock-expired-runs!
  [& {:keys [limit options] :as options}]
  (p/lock-expired-runs! @*storage* options))

