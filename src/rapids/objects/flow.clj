(ns rapids.objects.flow
  (:require [rapids.objects.address :as a]
            [rapids.support.util :refer [refers-to? qualify-symbol]]
            [rapids.support.defrecordfn :refer [defrecordfn]]))

(def ^:dynamic *defining-flows* [])

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

(defn flow-symbol? [o]
  (and (symbol? o)
    (or (refers-to? flow? o)
      (let [qsym (qualify-symbol o)]
        (some #(= % qsym) *defining-flows*)))))

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
