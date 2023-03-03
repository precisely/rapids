;;
;; Dynamics variables used by the storage system
;;
(ns rapids.storage.globals
  (:require [rapids.storage.protocol :as p]
            [rapids.support.debug])
  (:import (rapids.storage.protocol Storage)))

(def ^:dynamic *storage* nil)
(def ^:dynamic *connection* nil)
(def ^:dynamic *cache*
  "Cache is organized as a map from fully qualified class names (strings) to a map from
  ids (uuids) to object operation maps, containing :object and :op keys
  E.g.,
  { \"rapids.objects.Run\" {#uuid\"123...\" {:object #Run{:id #uudi\"123...\", :state...}
                                             :op :create},
                            #uuid\"a3f...\" ...
                            ...}
    \"rapids.objects.Pool\" ...}
  Note: the :op key indicates what should be done with the object at the end of a runlet.
  It is either: :create or :update. If an object is retrieved but not changed, its :op will be nil."
  nil)
(def ^:dynamic *strict-proxy*
  "Determines whether the CacheProxy object functions in strict mode (useful for production) or a more developer friendly mode.
  In non-strict mode the "
  false)

(defn current-connection
  "Returns the current connection to the current-storage or nil; typically set by with-connection or ensure-connection."
  [] *connection*)

(defn current-storage
  "Returns the current storage, an object supporting the Storage interface"
  [] *storage*)

(defn set-storage!
  "Sets the default storage. May be overridden with with-storage, optionally establishes a default connection and a top-level cache.

  Example:
  (set-storage s :connection true :cache true)"
  [storage & {:keys [connection cache]}]
  (alter-var-root #'*storage* (constantly storage))
  (if connection (alter-var-root #'*connection* (constantly (p/get-connection *storage*))))
  (if cache (alter-var-root #'*cache* (constantly {}))))

(defmacro with-storage
  "Override the existing storage. This may be useful if multiple storages are used."
  [storage & body]
  `(binding [*storage*    ~storage
             *connection* nil
             *cache*      nil]
     ~@body))

(defn get-connection
  [storage]
  {:pre [storage (satisfies? p/Storage storage)]}
  (p/get-connection storage))

(defmacro with-connection
  "Establishes a new connection with the current storage"
  [& body]
  `(binding [*connection* (get-connection *storage*)
             *cache*      nil]
     (try ~@body
       (finally (p/close *connection*)))))

(defmacro ensure-connection
  "Ensures *connection* is a valid connection"
  [& body]
  `(letfn [(exec# [] ~@body)]
     (if *connection*
       (exec#)
       (with-connection (exec#)))))
