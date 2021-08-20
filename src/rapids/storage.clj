(ns rapids.storage
  (:require [rapids.storage.transaction :as t]
            [rapids.storage.connection :as c]
            rapids.storage.persistence
            potemkin))

(defn get-object [cls id]
  (if (t/cache-exists?)
    (t/get-object cls id)
    (c/get-record cls id)))

(defn lock-object! [cls id]
  (if (t/cache-exists?)
    (t/lock-object! cls id)
    (c/lock-record! cls id)))

(defn update-object! [inst]
  (if (t/cache-exists?)
    (t/update-object! inst)
    (c/update-record! inst)))

(defn create-object! [inst]
  (if (t/cache-exists?)
    (t/create-object! inst)
    (c/create-record! inst)))

(defn current-connection
  "Returns the current connection to the current-storage or nil; typically set by with-connection or ensure-connection."
  [] c/*connection*)

(defn current-storage
  "Returns the current storage, an object supporting the Storage interface"
  [] c/*storage*)

(potemkin/import-vars
  [rapids.storage.connection with-storage set-storage! with-connection ensure-connection]
  [rapids.storage.persistence freeze thaw]
  [rapids.storage.transaction with-transaction])
