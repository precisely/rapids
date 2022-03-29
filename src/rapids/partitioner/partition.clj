(ns rapids.partitioner.partition
  (:require [rapids.partitioner.partition-utils :refer [dynamic?]]
            [rapids.partitioner.resume-at :refer [resume-at]])
  (:import (clojure.lang PersistentVector Keyword)))

;;;
;;; A Partition is a signature which will be used to create a partition function
;;;
(defrecord Partition
  [^PersistentVector params
   ^PersistentVector body
   ^Keyword type])

(def partition-type? #{;; simple partition without any suspending forms
                         :valued
                         ;; partition with a suspending form, but not resumed yet
                         ;; this partition cannot be extended, but may be passed to add-resumed
                         :suspending
                         ;; a partition containing one or more resume-at expressions
                         ;; this partition cannot be extended or passed to add-resume-at
                         :resumed})


(defn ->partition
  [params body type]
  {:pre [(sequential? params) (vector? body) (partition-type? type)]}
  (->Partition (vec params) body type))

(defn partition? [o] (instance? Partition o))

(defn body-value-expr
  "Returns an expression that returns the value of the partition's body
  (either `(do ~@body)` or an expr if the body contains a single expression."
  [p]
  {:pre [(partition? p) (-> p :type (= :valued))]}
  (let [body (-> p :body)]
    (if (-> body count (> 1))
      `(do ~@body)
      (first body))))

(defn extend-body [p type exprs]
  {:pre [(-> p :type (= :valued)) (partition-type? type)]}
  (-> (update p :body (comp vec concat) exprs)
    (assoc :type type)))

(defn- update-body
  "Updates the partition body, ensuring only non-suspending partitions may be updated."
  [p type f & args]
  {:pre [(-> p :type (not= :resumed)) (partition-type? type)]}
  (letfn [(safe-update-body [b]
            (let [result (apply f b args)]
              (assert (vector? result)
                "update-body function must return a vector")
              result))]
    (-> (update p :body safe-update-body)
      (assoc :type type))))

(defn add-resume-at
  "Resumes the partition at the given address, binding the body value to the param, if given."
  ([p addr] (add-resume-at p addr nil))
  ([p addr param]
   {:pre [(partition? p) (-> p :type (= :suspending))]}
   (update-body p :resumed
     (fn [body]
       [`(resume-at [~addr ~(:params p) ~param]
           ~@body)]))))

(defn partition-fn-def
  "Returns the code which defines the partition fn at address"
  [pmap address index]
  (letfn [(body-with-dynamic-bindings [bindings body]
            (if (empty? bindings) body
              `((binding ~bindings ~@body))))]
    (let [cdef             (get pmap address)
          name             (symbol (str (name (:flow address)) index))
          params           (:params cdef)
          dynamics         (filter dynamic? params) ; TODO: disallow binding system dynamic vars - security issue
          dynamic-bindings (vec (flatten (map #(vector % %) dynamics)))]
      `(fn ~name [{:keys ~params}]
         ~@(body-with-dynamic-bindings dynamic-bindings (:body cdef))))))
