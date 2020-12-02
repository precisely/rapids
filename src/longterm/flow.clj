(ns longterm.flow
  (:require [longterm.address :as address]
            [longterm.util :refer [refers-to?]]
            [longterm.defrecordfn :refer [defrecordfn]]
            [longterm.address :as a]
            [taoensso.nippy :refer [extend-freeze extend-thaw]]))

(defrecordfn Flow
  [;; Global symbol defined as this flow
   name
   ;; Function with arbitrary signature
   entry-point
   ;; A map of address-point strings to functions of the form (fn [{:keys [...]}])
   continuations
   ;; For debugging purposes:
   partitions]
  (fn [this & _] (throw (Exception. (str "Attempt to invoke flow " (:name this) " outside of run context."))))

  Object
  (toString [_] (format "#<Flow %s (%d partitions)>" name (count continuations))))

(defn flow? [o]
  (instance? Flow o))

(defn exec
  "Executes the flow partition at the address with the given bindings"
  ([address bindings]
   {:pre [(a/address? address)
          (map? bindings)]}
   (let [flow (address/resolved-flow address)
         partition (get-in flow [:partitions address])
         continuation (get-in flow [:continuations address])]
     (if-not (fn? continuation)
       (throw (Exception. (format "Attempt to continue flow at undefined partition %s" address))))
     (continuation bindings))))

(defn entry-point
  [flow args]
  (cond
    (flow? flow) (apply (get flow :entry-point) args)
    (symbol? flow) (recur (resolve flow) args)
    (var? flow) (recur (var-get flow) args)
    :else (throw (Exception. (format "Invalid flow %s" flow)))))

(defmethod print-method Flow
  [o w]
  (print-simple
    (str "#<Flow " (:name o) ">")
    w))

;;
;; nippy
;;
(extend-freeze Flow ::flow ; A unique (namespaced) type identifier
  [x data-output]
  (.writeUTF data-output (str (:name x))))

(extend-thaw ::flow ; Same type id
  [data-input]
  (let [flow-name (.readUTF data-input)]
    (var-get (resolve (symbol flow-name)))))
