(ns rapids.objects.flow
  (:require [rapids.objects.address :as a]
            [rapids.support.util :refer [refers-to? qualify-symbol]]
            [rapids.support.defrecordfn :refer [defrecordfn]]))

(defrecordfn Flow
  [;; Global symbol defined as this flow
   name
   ;; Function with arbitrary signature
   entry-point
   ;; A map of address-point strings to functions of the form (fn [{:keys [...]}])
   continuations
   ;; For debugging purposes:
   partitions]
  (fn [this & _]
    (throw (ex-info (str "Improperly invoked flow: " (:name this) ". Use start!, fcall or fapply when flow is bound dynamically.")
             {:type :runtime-error})))

  Object
  (toString [_] (format "#<Flow %s (%d partitions)>" name (count continuations))))

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

(defn exec
  "Executes the flow partition at the address with the given bindings"
  ([address bindings]
   {:pre [(a/address? address)
          (map? bindings)]}
   (let [flow (a/resolved-flow address)
         continuation (get-in flow [:continuations address])]
     (if-not (fn? continuation)
       (throw (ex-info (str "Attempt to continue flow at undefined partition " address)
                {:type :system-error})))
     (continuation bindings))))

(defn entry-point
  [flow args]
  (cond
    (flow? flow) (apply (get flow :entry-point) args)
    (symbol? flow) (recur (resolve flow) args)
    (var? flow) (recur (var-get flow) args)
    :else (throw (ex-info (str "Attempt to invoke Flow entry-point for object of type " (type flow))
                   {:type :runtime-error}))))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) ">")
    w))
