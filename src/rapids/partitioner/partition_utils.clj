(ns rapids.partitioner.partition-utils
  (:require [clojure.set :refer [difference intersection]]
            [clojure.walk :refer [postwalk]]
            [rapids.objects.address :as a]
            [rapids.support.util :refer [unqualified-symbol?]]
            [clojure.string :as str]))

;;
;; HELPERS
;;
(defn bindings-expr-from-params
  "Returns an expression that builds the hashmap {:a b, :c d,...} given two args, [a, c,...] [b, d...]
  or returns {:a a, :b b,..} given arg [a,b...]"
  ([params] (bindings-expr-from-params params params))
  ([key-params, arg-params]
   `(hash-map ~@(interleave (map keyword key-params) arg-params))))

(defn constant? [o]
  (or (number? o) (boolean? o) (string? o) (keyword? o)
    (and (or (vector? o) (set? o)) (every? constant? o))
    (and (map? o)
      (every? constant? (keys o))
      (every? constant? (vals o)))))

(defn parameter-symbol [address]
  (symbol (a/point-to-string address)))

(defn make-implicit-parameters [address args]
  (map-indexed (fn [i v]
                 (if (or (constant? v) (symbol? v))
                   v
                   (parameter-symbol (a/child address i))))
    args))

(def ^:dynamic *partitioning-expr* nil)
(defn throw-partition-error
  ([msg & args]
   (let [m      (meta *partitioning-expr*)
         line   (:line m)
         loc    (if line (format " at line %s" line) "")
         title  (format "Failed while partitioning %s%s. " *partitioning-expr* loc)
         detail (if msg (apply format msg args) "")]
     (throw (ex-info (str title detail)
              (assoc m
                :type :compiler-error))))))

(defn unqualified-symbols-in [form]
  "Returns a set of unqualified symbols appearing in form"
  (set (filter unqualified-symbol?
         (postwalk #(cond
                      (seq? %) (flatten %)
                      (set? %) (flatten (vec %))
                      (map? %) (flatten (map identity %))
                      :else %) form))))

(defn closure-captured-bindings
  "Given a form and a vector of unqualified symbols, returns the symbols which appear in form"
  [fn-params fn-body env-params]
  (let [form-symbols      (unqualified-symbols-in fn-body)
        body-bindings     (intersection form-symbols (set env-params))
        captured-bindings (difference body-bindings (set fn-params))]
    (vec captured-bindings)))

