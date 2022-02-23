(ns rapids.objects.version
  (:require [taoensso.nippy :as nippy]
            [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]
            [lein-project-reader.core :refer [read-project]]
            [clojure.string :as str]))

(declare string->version)

(def ^:dynamic *rapids-module-version*)

(defrecord Version
  [^Integer major,
   ^Integer minor,
   ^Integer patch,
   ^Boolean snapshot])

(defn module-version
  "Return the module"
  ([level]
   {:pre [(#{:major :minor :patch :snapshot} level)]}
   (level *rapids-module-version*))
  ([]
   {:post [(instance? Version %)]}
   (if (bound? #'*rapids-module-version*) *rapids-module-version*
     (alter-var-root #'*rapids-module-version* (constantly (-> (read-project) :version string->version))))))

(defn version->string [v]
  (str (:major v) "." (:minor v) "." (:patch v) (if (:snapshot v) "-SNAPSHOT" "")))

(defn string->version [v]
  (let [[major minor patch] (str/split v #"\.")
        [patch snapshot?] (str/split patch #"\-")
        major     (Integer/parseInt major)
        minor     (Integer/parseInt minor)
        patch     (Integer/parseInt patch)
        snapshot? (boolean (and (string? snapshot?) (-> snapshot? str/upper-case (= "SNAPSHOT"))))]
    (->Version major minor patch snapshot?)))

(defmacro version [versionstr & body]
  {:pre [(string? versionstr)]}
  `(binding [*rapids-module-version* ~(string->version versionstr)]
     ~@body))

(defmethod print-method Version
  [o w]
  (print-simple (str "(->version \"" (version->string o) "\")") w))