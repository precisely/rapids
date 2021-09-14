;;
;; This file represents the interface of the storage system to the rest of the code
;; Do not reference
;;
(ns rapids.storage
  (:require rapids.storage.cache
            rapids.storage.connection-wrapper
            rapids.run
            rapids.pool
            [potemkin :refer [import-vars]])
  (:import (rapids.run Run)
           (rapids.pool Pool)))

(potemkin/import-vars
  [rapids.storage.dynamics
   with-storage set-storage! with-connection ensure-connection current-storage current-connection]
  [rapids.storage.connection-wrapper
   get-records! find-records! update-records! create-records!
   get-record! find-record! update-record! create-record!]
  [taoensso.nippy freeze thaw extend-freeze extend-thaw
   freeze-to-out! thaw-from-in!]
  [rapids.storage.cache
   ensure-cached-connection cache-create! cache-update! cache-get! cache-find! cache-exists?
   cache-find!])
