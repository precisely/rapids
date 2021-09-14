(ns helpers
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [rapids.storage :refer :all])
  (:import (java.util Properties)
           (rapids.run Run)
           (rapids.pool Pool)))

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

;; easy access functions which
(defn get-run [id] (ensure-connection (get-record! Run id)))
(defn get-pool [id] (ensure-connection (get-record! Pool id)))