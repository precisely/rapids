(ns rapids.objects.version
  (:require [clojure.string :as str]))

(declare string->version)

(defrecord Version
  [^Integer major,
   ^Integer minor,
   ^Integer patch,
   ^Boolean snapshot]
  Object
  (toString [this] (pr-str this)))

(def ^:dynamic *current-version* (->Version 0 0 0 false))

(defn current-version
  ([level]
   {:pre [(#{:major :minor :patch :snapshot} level)]}
   (level *current-version*))
  ([]
   {:post [(instance? Version %)]}
   *current-version*))

(defn- flatten-version [{major :major, minor :minor, patch :patch}] [major minor patch])

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
(def version= =)

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

(defn version->vector [v]
  (vec (map v [:major :minor :patch :snapshot])))

(defn version->string [v]
  (str (:major v) "." (:minor v) "." (:patch v) (if (:snapshot v) "-SNAPSHOT" "")))

(defn version? [o]
  (and o (instance? Version o)))

(defn ->version [o]
  (cond
    (string? o) (string->version o)
    (seq? o) (apply ->Version o)
    (version? o) o
    :else (throw (ex-info "Unexpected value to ->version. Expecting string or sequence"
                   {:type   :fatal-error
                    :object o}))))

(defmethod print-method Version
  [o w]
  (print-simple (str "(->version \"" (version->string o) "\")") w))