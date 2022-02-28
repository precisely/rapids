(ns rapids.objects.module
  (:require [rapids.objects.version :refer :all]
            [clojure.string :as str]
    ;[org.apache.commons.io.IOUtils :as IOUtils]
            )
  (:import (clojure.lang Namespace RT)
           (org.apache.commons.io IOUtils)))

(def ^:dynamic *ns-module-index*
  "A map from namespace identifiers to modules"
  (atom {"" 'rapids/root}))

(def +default-version+ (->version "0.0.1"))

(def +modules+
  "A map from fully qualified symbols to Module objects"
  (atom {}))

(defrecord Module [name doc version-finder])

(defn ns-identifier
  "Returns an array of strings, representing the components of the namespace name.

  (ns foo.bar.baz)
  (ns-identifier *ns*) => [\"foo\" \"bar\" \"baz\"]
  (ns-identifier \"a.b.c\") => [\"a\" \"b\" \"c\"]"
  [ns]
  (cond
    (string? ns) (str/split ns #"\.")
    (symbol? ns) (recur (-> ns name))
    (instance? Namespace ns) (recur (.getName ns))))

(defn- -find-module [ns-id]
  {:pre [(sequential? ns-id)]}
  (or (get *ns-module-index* ns-id)
    (if-not (empty? ns-id)
      (recur (butlast ns-id)))))

(defn current-module
  ([] (current-module *ns*))
  ([ns] (-find-module (ns-identifier ns))))


(defn set-module!
  "Associates a module symbol with a namespace (using the current namespace by default).
  All children of the namespace are considered part of the module. A module can be set for
  multiple namespaces."
  ([sym] (set-module! sym *ns*))
  ([sym ns]
   {:pre [(qualified-symbol? sym)]}
   (let [ns-id (ns-identifier ns)]
     (if-let [existing (get @*ns-module-index* ns-id)]
       (if-not (= existing sym)
         (println "Warning: replacing namespace module " existing " with " sym " in namespace " ns)))
     (swap! *ns-module-index* update ns-id sym))))
;;
;;(defmacro with-module
;;  "Establishes a temporary module and version context for code body."
;;  [[name] & body]
;;  {:pre [(qualified-symbol? name)]}
;;  `(binding [*ns-module-index* (atom @*ns-module-index*)]
;;     (set-module! '~name)
;;     ~@body))

(defmacro ^{:arglists '([name docstring? & {:keys [ns versions]}])}
  defmodule [name & args]
  (let [name (if (qualified-symbol? name)
               name (rapids.support.util/qualify-symbol name))
        [doc-string? kwargs?] args
        [doc-string & {:keys [versions]}]
        (if (string? doc-string?) [doc-string? kwargs?] [nil args])]
    (swap! +modules+ assoc name (->Module name doc-string versions))
    name))

(defmodule :precisely/anticoagulation
  :ns 'pia-server.apps.anticoagulation ; internal location
  :flows [a b c d e]
  :versions
  (constantly
    [["0.1" _ 'pia-server.apps.anticoagulation.versions.0.1]
     (fn [ns-name major minor flow-name]
       [(symbol (str (name ns-name) ".versions." major "-" minor ".core")) flow-name])]))


(defn subdir-versions [minor-version-map]
  (fn [major]
    ))

(defn- ns-path
  [ns]
  (-> (ns-name ns)
    name
    (str/replace \- \_)
    (str/replace \. \/)))

(defn- resolve-path [path]
  (.getResource (RT/baseLoader) path))

(defn ns-to-path [ns])
(defn source-clj
  [ns]
  (require ns)
  (some->> ns
    ns-publics
    vals
    first
    meta
    :file
    #_#_(.getResourceAsStream (RT/baseLoader))
            IOUtils/toString))