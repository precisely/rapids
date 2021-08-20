(ns rapids.storage.transaction
  (:require [rapids.storage.connection :as c]
            [rapids.storage.protocol :as p]
            [rapids.util :refer :all]))

(def ^:dynamic *cache* nil
  "When bound, the cache is an atom of a map that looks like this:
  (atom { class1
          { id1
            {:object atom(instance-of-class1),
             :cache-state cache-state}
             ...}
          class2
          { id2
            {:object atom(instance-of-class2), ...)
             :cache-state ...}
            ...}})")

(defn cache-exists? [] (boolean *cache*))

(defn save-cache! []
  (doseq [[cls entries] *cache*]
    (doseq [[id {object :object cache-change :op}] entries]
      (assert (atom? object))
      (when cache-change
        (case cache-change
          ;; TODO: maybe optimize using bulk create/update instead
          :update (c/update-record! object)
          :create (c/create-record! object))
        (swap! *cache* dissoc-in [cls id :op])))))

(declare get-object get-cache-entry set-cache-entry)

(defn lock-object!
  ""
  [cls id]
  {:pre [(cache-exists?)
         (class? cls)]}
  (let [{existing :object locked :locked} (get-cache-entry cls id)]
    (if locked
      existing
      (set-cache-entry (c/lock-record! cls id) :locked true))))

(defn update-object!
  "Updates the object, setting the :op state appropriately: :created objects stay in :created state,
  changed objects are put in :update state."
  [inst]
  {:pre [(cache-exists?)
         (atom? inst)]}
  (let [id (:id @inst)
        cls (class @inst)
        {existing :object existing-change :op} (get-cache-entry cls id)
        cache-change (if existing (or existing-change :update))]
    (set-cache-entry inst :op cache-change)))

(defn update-object2!
  "Updates the object, setting the :op state appropriately: :created objects stay in :created state,
  changed objects are put in :update state."
  [inst & kvs]
  {:pre [(cache-exists?)
         (atom? inst)]}
  (let [id (:id @inst)
        cls (class @inst)
        {existing :object existing-change :op} (get-cache-entry cls id)
        cache-change (if existing (or existing-change :update))]
    ()
    (set-cache-entry inst :op cache-change)))

(defn create-object!
  [inst]
  {:pre [(cache-exists?)
         (atom? inst)]}
  (let [id (:id @inst)
        cls (class @inst)]
    (assert (not (get-cache-entry cls id)) (str "Attempt to create object in cache which already exists: " (.getName cls) id))
    (set-cache-entry inst :op :create)))

(defmacro with-transaction
  "Executes body within a transaction generating a new connection for that transaction within the storage.
  Note: operations on the storage are performed as a final step."
  [& body]
  `(letfn [(exec# [] ~@body)]
     (c/ensure-connection
       (if *cache*
         (exec#)
         (binding [*cache* (atom {})]
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
(defn- get-cache-entry [cls id]
  (get *cache* [cls id]))

(defn- set-cache-entry [inst & {:keys [op locked]}]
  (let [cls (class @inst)
        id (:id @inst)
        locked (or locked (:locked (get-cache-entry cls id)))]
    (swap! *cache* update-in [cls id]
      {:object inst
       :op     op
       :locked locked})))
