(ns rapids.objects.flow
  (:require [rapids.objects.address :as a]
            [rapids.support.defrecordfn :refer [defrecordfn]]
            [rapids.support.util :refer [qualify-symbol refers-to?]]
            [rapids.objects.startable :as startable])
  (:import (clojure.lang Named IPersistentMap Symbol)
           (rapids.objects.startable Startable)
           (rapids.objects.version Version)))

(declare call-entry-point)
(defrecordfn Flow
  [;; Global symbol defined as this flow
   ^Symbol name

   ^Symbol module

   ^Version version

   ^String doc

   ;; Map of major version numbers to functions with user-defined signatures
   ^IPersistentMap entry-points

   ;; Map of major version numbers to entry point parameter vectors
   ^IPersistentMap entry-point-parameters

   ;; Map of addresses to functions of the form (fn [{:keys [...]}])
   ^IPersistentMap partition-hashes

   ;; Map of addresses to partition parameter lists
   ^IPersistentMap partition-params

   ;; Map of partition hashes to partition functions
   ^IPersistentMap partition-fns]

  :fn (fn [this & _]
        (throw (ex-info (str "Improperly invoked flow: " (:name this) ". Use start!, fcall or fapply when flow is bound dynamically.")
                 {:type :runtime-error})))

  Object
  (toString [this] (format "#<Flow %s (%d partitions)>" (:name this) (-> this :partition-fns count)))

  Named
  (getNamespace [this] (.getNamespace (:name this)))
  (getName [this] (-> this :name name))

  Startable
  (flow-name [this] (:name this))
  (version [this] (:version this))
  (module [this] (:module this))
  (requirements [this ])
  (begin [this args major-version] (call-entry-point this args major-version)))

(defn flow? [o]
  (instance? Flow o))

(def ^:dynamic *defining-flows*
  "The set of all flows currently being defined. This is not currently used for multiple flows, but
  may be in the future with a (flow-let [(f1...) (f2...)] ) macro to allow defining mutually recursive flows"
  #{})

(defmacro with-flow-definitions
  "Marks a symbol (name) as binding a flow (which is being defined in body). This tells
  the partitioner to treat invokation of the symbol as a suspending expression."
  [names & body]
  `(binding [*defining-flows* (clojure.set/union *defining-flows* (set (map qualify-symbol (if (seq? ~names) ~names [~names]))))]
     ~@body))

(defn in-flow-definition-context? []
  (-> *defining-flows* count (> 0)))

(defn flow-symbol? [o]
  (and (symbol? o)
    (or (refers-to? flow? o)
      (*defining-flows* (qualify-symbol o)))))

(defn call-partition
  "Executes the partition function at address with the given bindings"
  [address bindings]
  {:pre [(a/address? address)
         (map? bindings)]}
  (let [flow           (a/resolved-flow address)
        partition-hash (get-in flow [:partition-hashes address])
        pfn            (some->> (get-in flow [:partition-fns partition-hash])
                         resolve var-get)]
    (if-not (fn? pfn)
      (throw (ex-info (str "Attempt to continue flow at undefined partition " address)
               {:type        :system-error
                :object      pfn
                :object-type (type pfn)})))
    (pfn bindings)))

(defn call-entry-point [this args major-version]
  {:pre [(flow? this) (seq? args) (if major-version (number? major-version))]}
  (let [major-version (or major-version (-> this startable/version :major))]
    (if-let [entry-point (get-in this [:entry-points major-version])]
      (apply entry-point args)
      (throw (ex-info (str "Attempt to call flow entry point. Major version level unsupported: "
                        major-version)
               ({:type             :fatal-error
                 :flow-name        (:name this)
                 :supported-levels (-> this :entry-points keys)
                 :requested-level  major-version}))))))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) ">")
    w))

