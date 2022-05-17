(ns rapids.partitioner.partition-utils
  (:require [clojure.set :refer [difference intersection]]
            [clojure.walk :refer [postwalk]]
            [rapids.support.util :refer [unqualified-symbol?]]))

;;
;; HELPERS
;;
(defn bindings-expr-from-params
  "Returns an expression that builds the hashmap {:a b, :c d,...} given two args, [a, c,...] [b, d...]
  or returns {:a a, :b b,..} given arg [a,b...]"
  ([params] (bindings-expr-from-params params params))
  ([key-params, arg-params]
   (apply hash-map (interleave (map keyword key-params) arg-params))))

(defn constant? [o]
  (or (number? o) (boolean? o) (string? o) (keyword? o)
      (and (or (vector? o) (set? o)) (every? constant? o))
      (and (map? o)
           (every? constant? (keys o))
           (every? constant? (vals o)))))

(defn var-expr? [o]
  (and (seq? o)
       (-> o count (= 2))
       (-> o first (= 'var))))

(defn quote-expr? [o]
  (and (seq? o)
       (-> o count (= 2))
       (-> o first (= 'quote))))

(def ^{:arglists '([e])} constant-expr?
  "True if the expr e is guaranteed to produce no side-effects. Evaluation of such forms may be delayed until a later
  stage of computation."
  (some-fn constant? symbol? var-expr? quote-expr?))

(def ^:dynamic *partitioning-expr* nil)
(defn throw-partition-error
  ([msg & args]
   (let [m (meta *partitioning-expr*)
         line (:line m)
         loc (if line (format " at line %s" line) "")
         title (format "Failed while partitioning %s%s. " *partitioning-expr* loc)
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
  (let [form-symbols (unqualified-symbols-in fn-body)
        body-bindings (intersection form-symbols (set env-params))
        captured-bindings (difference body-bindings (set fn-params))]
    (vec captured-bindings)))

(defn dynamic? [o]
  (and (symbol? o)
       (resolve o)
       (-> o meta :dynamic)))

(defn add-params [params & new-params]
  {:pre [(sequential? params) (every? simple-symbol? new-params)]}
  (vec (distinct (concat params new-params))))

(defn make-let-body
  "Returns a vector. If bindings is not empty, returns a vector containing a let expression lexically binding the bindings,
  otherwise returns the body as a vector.

  bindings in the form of 2-tuples [sym expr] "
  [bindings body]
  {:pre [(sequential? bindings)
         (every? (comp simple-symbol? first) bindings)
         (every? #(-> % count (= 2)) bindings)
         (sequential? body)]}
  (let [filtered-bindings (filter (fn [[s v]] (not= s v)) bindings) ; eliminate rebinding the same symbol
        let-bindings (vec (apply concat filtered-bindings))]
    (if (-> let-bindings count (> 0))
      [`(let [~@let-bindings] ~@body)]
      (vec body))))

(defn params-from-args
  "Reads an arglist, returning a vector of simple symbols representing the parameters."
  ([args] (params-from-args args []))
  ([[arg & args] params]
   (cond
     (= '& arg) (let [[variadic & args] args]
                  (assert (empty? args) "Too many variadic arguments")
                  (cond
                    (map? variadic)
                    (cond-> (apply add-params params (:keys variadic))
                            (contains? variadic :as) (add-params (:as variadic)))
                    (simple-symbol? variadic)
                    (add-params params variadic)
                    :else (assert false (str "Bad variadic argument" variadic))))
     (simple-symbol? arg) (recur args (add-params params arg))
     (map? arg) (recur args (cond-> (apply add-params params (keys (dissoc arg :as)))
                                    (contains? arg :as) (add-params (:as arg))))
     (nil? arg) params)))

;; method used by pre-node-based partitioner
;;(defn make-let-body [bindings body]
;;  {:pre [(vector? body)]}
;;  (let [filtered-bindings (filter (fn [[k v]]
;;                                    (if (constant? k) ; accept [asdf :foo] but not [:foo asdf]
;;                                      (assert (= k v)) ; returns nil
;;                                      [k v]))
;;                            bindings)
;;        let-bindings      (vec (apply concat filtered-bindings))]
;;    (if (-> let-bindings count (> 0))
;;      [`(let [~@let-bindings] ~@body)]
;;      body)))