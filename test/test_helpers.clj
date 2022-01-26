(ns test-helpers
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [rapids.storage.core :as s]
            [rapids.objects.run :as r]
            rapids.objects.pool
            [rapids.runtime.runlet :as runlet]
            [spy.core :as spy]
            [potemkin :refer [import-vars]]
            [rapids.storage.core :as storage]
            rapids.language.test
            [potemkin :refer [import-vars]])
  (:import (java.util Properties)
           (rapids.objects.run Run)
           (rapids.objects.pool Pool)))

(import-vars
  [rapids.language.test with-test-storage with-test-env flush-cache!])

(defn run-in-state? [r state]
  (storage/ensure-cached-connection
    (= state (:state r))))

(defn proxy-field [p & ks]
  (storage/ensure-cached-connection
    (get-in p ks)))

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

(defmacro with-test-env-run
  "Provides a test environment and a run"
  [bindings & body]
  {:pre [(vector? bindings)]}
  `(with-test-env
     (runlet/with-run (s/cache-insert! (r/make-run))
       (let [~@bindings]
         ~@body))))

(defmacro with-continue!-stub
  "Binds a spy/stub to stub which always returns return-value, redefining the continue!"
  [[stub return-value] & body]
  `(let [~stub (spy/stub ~return-value)]
     (with-redefs [rapids.runtime.run-loop/continue! ~stub]
       ~@body)))

;; easy access functions
(defn get-run-record
  "Get run directly from storage"
  [id]
  (s/ensure-connection (s/get-record! Run id)))

(defn get-pool-record
  "Get pool directly from storage"
  [id]
  (s/ensure-connection (s/get-record! Pool id)))