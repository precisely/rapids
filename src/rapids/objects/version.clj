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
   ^Boolean snapshot]
  Object
  (toString [this] (pr-str this)))

(defn module-version
  "Return the module"
  ([level]
   {:pre [(#{:major :minor :patch :snapshot} level)]}
   (level *rapids-module-version*))
  ([]
   {:post [(instance? Version %)]}
   (if (bound? #'*rapids-module-version*) *rapids-module-version*
     (-> (read-project) :version string->version))))

(defn flatten-version [{major :major, minor :minor, patch :patch}] [major minor patch])

(defn num-sequence>
  "Tests two sequences of numbers from left to right, returning true if s1>s2"
  [s1 s2]
  (let [[s1num & s1rest] s1
        [s2num & s2rest] s2
        s1num (or s1num 0)
        s2num (or s2num 0)]
    (if (> s1num s2num) true
      (if (< s1num s2num) false
        (if (or s1rest s2rest)
          (num-sequence> s1rest s2rest)
          false)))))

(defn version> [v1 v2] (num-sequence> (flatten-version v1) (flatten-version v2)))

(defn version->string [v]
  (str (:major v) "." (:minor v) "." (:patch v) (if (:snapshot v) "-SNAPSHOT" "")))

(defn version-change
  "Returns [level v1-level v2-level] if versions differ, where level = :major|:minor|:patch"
  [v1 v2]
  (-> (drop-while (fn [[_ v1n v2n]] (= v1n v2n)) (map #(vector %1 %2 %3) [:major :minor :patch :snapshot] (flatten-version v1) (flatten-version v2)))
    first))

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