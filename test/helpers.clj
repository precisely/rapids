(ns helpers
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [rapids.storage.core :as s]
            [rapids.objects.run :as r]
            [rapids.runtime.runlet :as runlet]
            [rapids.implementations.in-memory-storage :as imrs]
            [spy.core :as spy]
            [potemkin :refer [import-vars]])
  (:import (java.util Properties)
           (rapids.objects.run Run)
           (rapids.objects.pool Pool)))

(import-vars
  [rapids.runtime.core current-run]
  [rapids.storage.core with-storage]
  [rapids.implementations.in-memory-storage ->in-memory-storage])

(defmacro with-temp-ns [& body]
  `(let [cur-ns# (symbol (str (.getName *ns*)))]
     (in-ns '~(gensym))
     (try
       ~@body
       (finally (in-ns cur-ns#)))))

(def env
  (->> (merge
         (into {} (System/getenv))
         (into {} (System/getProperties))
         (let [env-file (io/file ".env")]
           (if (.exists env-file)
             (let [props (Properties.)]
               (.load props (io/input-stream env-file))
               props)
             {})))
    (map (fn [[k v]] [(-> (str/lower-case k)
                        (str/replace "_" "-")
                        (str/replace "." "-")
                        (keyword))
                      v]))
    (into {})))

(defmacro throws-error-output [regex form]
  `(re-find ~regex (try ~form
                        (catch Exception e#
                          (str e#)))))

(defmacro with-test-storage [& body]
  `(s/with-storage (imrs/->in-memory-storage)
     ~@body))

(defmacro with-runtime-env [[& bindings] & body]
  `(s/ensure-cached-connection
     (runlet/with-run (s/cache-create! (r/make-run))
       (let [~@bindings]
         ~@body))))

(defmacro with-continue!-stub [[stub return-value] & body]
  `(let [~stub (spy/stub ~return-value)]
     (with-redefs [rapids.runtime.run-loop/continue! ~stub]
       ~@body)))

;; easy access functions
(defn get-run
  "Get run directly from storage"
  [id]
  (s/ensure-connection (s/get-record! Run id)))

(defn get-pool
  "Get pool directly from storage"
  [id]
  (s/ensure-connection (s/get-record! Pool id)))