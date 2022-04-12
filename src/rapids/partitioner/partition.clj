(ns rapids.partitioner.partition
  (:require [rapids.partitioner.partition-utils :refer [dynamic? throw-partition-error]]
            [rapids.partitioner.resume-at :refer [resume-at]])
  (:import (clojure.lang PersistentVector Keyword)))

;;;
;;; A Partition is a signature which will be used to create a partition function
;;;
(defrecord Partition
  [^PersistentVector params
   ^PersistentVector body
   ^Keyword suspending])

(defn ->partition
  [params body suspending]
  {:pre [(sequential? params) (sequential? body) (boolean? suspending)]}
  (->Partition (vec params) body suspending))

(defn partition? [o] (instance? Partition o))

(defn suspending? [p]
  {:pre [(partition? p)]}
  (:suspending p))

(defn body-value-expr
  "Returns an expression that returns the value of the partition's body
  (either `(do ~@body)` or an expr if the body contains a single expression."
  [p]
  {:pre [(partition? p) (not (suspending? p))]}
  (let [body (-> p :body)]
    (if (-> body count (> 1))
      `(do ~@body)
      (first body))))

(defn extend-body [p suspending exprs]
  {:pre [(not (suspending? p)) (boolean? suspending)]}
  (-> (update p :body (comp vec concat) exprs)
    (assoc :suspending suspending)))

(defn ^{:arglists '([p f & args] [p suspending f & args])
        :doc      "Updates the partition body

              p - partition
              suspending - (optional) the new partition suspending boolean
              f - unary function takes the existing body, returns a new body"}
  update-body
  [p & args]
  {:pre [(partition? p)]}
  (let [[_suspending? _f? & args] args
        [suspending f args] (if (fn? _suspending?)
                        [(suspending? p) _suspending? `(~_f? ~@args)]
                        [_suspending? _f? args])]
    (assert (boolean? suspending))
    (assert (fn? f))
    (letfn [(safe-update-body [b]
              (let [result (apply f b args)]
                (assert (vector? result)
                  "update-body function must return a vector")
                result))]
      (-> (update p :body safe-update-body)
        (update :suspending #(if (and % (not suspending))
                               (throw-partition-error "Attempt to set suspending partition to non-suspending" %)
                               suspending))))))

(defn add-resume-at
  "Resumes the partition at the given address, binding the body value to the param, if given."
  ([p addr] (add-resume-at p addr nil))
  ([p addr param]
   {:pre (suspending? p)}
   (update-body p true
     (fn [body]
       [`(resume-at [~addr ~(:params p) ~param]
           ~@body)]))))

(defn partition-fn-def
  "Returns the code which defines the partition fn at address"
  [partition address index]
  (letfn [(body-with-dynamic-bindings [bindings body]
            (if (empty? bindings) body
              `((binding ~bindings ~@body))))]
    (let [name             (symbol (str (name (:flow address)) index))
          params           (:params partition)
          body             (:body partition)
          dynamics         (filter dynamic? params) ; TODO: disallow binding system dynamic vars - security issue
          dynamic-bindings (vec (flatten (map #(vector % %) dynamics)))]
      `(fn ~name [{:keys ~params}]
         ~@(body-with-dynamic-bindings dynamic-bindings body)))))


(defn partition-printer [p]
  (str "(->partition '" (:params p) " '" (:body p) " " (:suspending p) ")"))

(defmethod print-method Partition
  [a w]
  (print-simple (partition-printer a) w))

(defmethod clojure.pprint/simple-dispatch Partition [o] (pr o))

