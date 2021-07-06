;;
;; Nippy provides serialization/deserialization for most data structures,
;; But some objects require special handling.
;;
(ns rapids.persistence
  (:require [taoensso.nippy :refer :all]
            [clojure.main :as main]
            [rapids.runlet-context :as rc]
            [rapids.pool :as p]
            [rapids.storage.core :as storage])
  (:import (rapids.run Run)
           (rapids.flow Flow)
           (clojure.lang AFunction)
           (rapids.pool Pool)))

;;
;; Run - always acquired from the storage, when the cache is available
;;
(extend-freeze Run ::run
  [run data-output]
  (let [run-id (prn-str (:id run))]
    (.writeUTF data-output run-id)))

(extend-thaw ::run
  [data-input]
  (let [thawed-str (.readUTF data-input)
        run-id     (read-string thawed-str)]
    (if (rc/runlet-cache-exists?)
      (rc/load-run! run-id)

      ;; special handling for when a run is retrieved outside of a run-cache context
      ;; just get the run without locking it or storing it in the cache
      (storage/get-run run-id))))

;;
;; Flow - flows contain functions which aren't defined at top level
;;        so they won't freeze without special handling
;;
(extend-freeze Flow ::flow    ; A unique (namespaced) type identifier
  [x data-output]
  (.writeUTF data-output (prn-str (:name x))))

(extend-thaw ::flow           ; Same type id
  [data-input]
  (let [flow-name (.readUTF data-input)]
    (var-get (resolve (read-string flow-name)))))

;;
;; Class
;;
(extend-freeze Class ::class
  [x data-output]
  (.writeUTF data-output (.getName x)))

(extend-thaw ::class
  [data-input]
  (-> data-input .readUTF Class/forName))

;;
;; AFunction
;;
(defn- pretty-demunge
  [fn-object]
  (let [dem-fn (main/demunge (str fn-object))
        pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem-fn))]
    (if pretty pretty dem-fn)))

(extend-freeze AFunction ::a-function
  [x data-output]
  (.writeUTF data-output (pretty-demunge x)))

(extend-thaw ::a-function
  [data-input]
  (-> data-input .readUTF symbol resolve var-get))


;;
;; Pool - always acquired from the storage, when the cache is available
;;
(extend-freeze Pool ::pool
  [pool data-output]
  (let [pool-id (prn-str (:id pool))]
    (.writeUTF data-output pool-id)))

(extend-thaw ::pool
  [data-input]
  (let [thawed-str (.readUTF data-input)
        pool-id     (read-string thawed-str)]
    (if (rc/runlet-cache-exists?)
      (rc/load-pool! pool-id)

      ;; special handling for when a run is retrieved outside of a run-cache context
      ;; just get the run without locking it or storing it in the cache
      (storage/get-pool pool-id))))
