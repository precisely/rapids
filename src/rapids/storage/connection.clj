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
(def ^:dynamic *cache* nil)
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

(defn create-records!
  [object]
  {:pre [(atom? object)
         (satisfies? p/Storage @*storage*)
         ()]}
  (p/create-records! @*storage* object))

(defn update-records!
  [obj]
  {:pre [(satisfies? p/Storage @*storage*)]}
  (p/update-records! @*storage* obj))

(defn get-records!
  ([type ids] (get-records! type ids false))
  ([type ids lock]
   {:post [(atom? %) (instance? type @%)]}
   (p/get-records! @*storage* type ids lock)))

(defn find-records!
  [type field & {:keys [eq lt gt lte gte eq limit lock in exclude] :as keys}]
  {:post [(every? #(and (atom? %) (instance? type @%)) %)]}
  (p/find-records! @*storage* type field keys))

