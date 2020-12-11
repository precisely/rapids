;;
;; Nippy provides serialization/deserialization for most data structures,
;; But some objects require special handling.
;;
(ns rapids.persistence
  (:require [taoensso.nippy :refer :all]
            [clojure.main :as main]
            [rapids.run-context :as rc]
            [rapids.runstore :as rs])
  (:import (rapids.run Run)
           (rapids.flow Flow)
           (clojure.lang AFunction)))

;;
;; Run - always acquired from the runstore, when the cache is available
;;
(extend-freeze Run ::run
  [run data-output]
  (let [run-id (prn-str (:id run))]
    (.writeUTF data-output run-id)))

(extend-thaw ::run
  [data-input]
  (let [thawed-str (.readUTF data-input)
        run-id     (read-string thawed-str)]
    (if (rc/run-cache-exists?)
      (rc/load! run-id)

      ;; special handling for when a run is retrieved outside of a run-cache context
      ;; just get the run without locking it or storing it in the cache
      (rs/get-run run-id))))

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
;; Pool
;;
()