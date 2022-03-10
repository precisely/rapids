(ns rapids.partitioner.resume-at
  (:require [rapids.objects.address :as a]
            [rapids.partitioner.partition-utils :refer [bindings-expr-from-params]]))
;;
;; resume-at
;;
(defmacro resume-at
  "Generates code that continues execution at address after body.
  address - names the partition
  params - list of parameters needed by the partition
  input-key - the key to which the value of form will be bound in the partition
  body - expression which may suspend the run

  Returns:
  value of body"
  ([[address params input-key] & body]
   (:pre [(a/address? address)
          (vector? params)
          (or (nil? input-key) (symbol? input-key))])
   `(let [bindings# ~(bindings-expr-from-params params)]
      (rapids.runtime.runlet/push-stack! ~address bindings# '~input-key)
      ~@body)))

(defn resume-at-expr? [o]
  (and (seq? o) (->> o first (contains? #{'resume-at `resume-at}))))

(defn resume-at-expr-data
  ([expr field]
   {:pre [(contains? #{:address :params :input-key :body} field)]}
   (field (resume-at-expr-data expr)))

  ([expr]
   {:pre [(resume-at-expr? expr)]}
   (let [[address params input-key] (second expr)]
     {:address address
      :params params
      :input-key input-key
      :body (nthrest expr 2)})))

(defn redirect-resume-at [e addr]
  {:pre [(resume-at-expr? e)]}
  (let [{params    :params
         input-key :input-key
         body      :body} (resume-at-expr-data e)]
    `(resume-at [~addr ~params ~input-key]
       ~@body)))
