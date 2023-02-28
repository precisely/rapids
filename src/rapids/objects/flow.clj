(ns rapids.objects.flow
  (:require [rapids.objects.address :as a]
            [rapids.objects.startable :as c]
            [rapids.support.defrecordfn :refer [defrecordfn]]
            [rapids.support.util :refer [qualify-symbol refers-to?]])
  (:import (clojure.lang Named)
           (rapids.objects.startable Startable)))

(defrecordfn Flow
  [;; Global symbol defined as this flow
   name
   ;; Function with arbitrary signature
   entry-point
   ;; A map of address-point strings to functions of the form (fn [{:keys [...]}])
   partition-fns]

  :fn (fn [this & _]
        (throw (ex-info (str "Improperly invoked flow: " (:name this) ". Use start!, fcall or fapply when flow is bound dynamically.")
                        {:type :runtime-error})))

  Object
  (toString [this] (format "#<Flow %s (%d partitions)>" (:name this) (-> this :partition-fns count)))

  Named
  (getNamespace [this] (.getNamespace (:name this)))
  (getName [this] (-> this :name name))

  Startable
  (c/call-entry-point [this args]
    (apply (get this :entry-point) args)))

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
  (let [flow (a/resolved-flow address)
        pfn  (get-in flow [:partition-fns (:point address)])]
    (if-not (ifn? pfn)
      (throw (ex-info (str "Attempt to continue flow at undefined partition " (prn-str address))
                      {:type        :system-error
                       :object      pfn
                       :object-type (type pfn)})))
    (pfn bindings)))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) ">")
    w))
