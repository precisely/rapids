;;
;; This file represents the interface of the storage system to the rest of the code
;; Do not reference
;;
(ns rapids.storage
  (:require rapids.storage.cache
            [rapids.storage.connection :refer [*connection* *storage*]]
            rapids.storage.persistence
            [potemkin :refer [import-vars]]))

(defn current-connection
  "Returns the current connection to the current-storage or nil; typically set by with-connection or ensure-connection."
  [] *connection*)

(defn current-storage
  "Returns the current storage, an object supporting the Storage interface"
  [] *storage*)

(potemkin/import-vars
  [rapids.storage.connection with-storage set-storage! with-connection ensure-connection]
  [rapids.storage.persistence freeze thaw]
  [rapids.storage.cache with-cache cache-create! cache-update! cache-get! cache-find!])
