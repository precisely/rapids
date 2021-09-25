;;
;; This file represents the interface of the storage system to the rest of the code
;; Do not reference
;;
(ns rapids.storage.core
  (:require rapids.storage.cache
            rapids.storage.connection-wrapper
            [potemkin :refer [import-vars]]))

(potemkin/import-vars
  [rapids.storage.globals
   with-storage set-storage! with-connection ensure-connection current-storage current-connection]
  [rapids.storage.connection-wrapper
   get-records! find-records! update-records! create-records!
   get-record! find-record! update-record! create-record!]
  [taoensso.nippy freeze thaw extend-freeze extend-thaw
   freeze-to-out! thaw-from-in!]
  [rapids.storage.cache ->CacheProxy cache-proxy?
   cache-proxy? ensure-cached-connection cache-insert! cache-get! cache-find! cache-exists?
   cache-find!])
