(ns rapids.partition-utils
  (:require [rapids.util :refer [ifit in? unqualified-symbol?]]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [intersection difference]]))

;;
;; HELPERS
;;
(defn bindings-expr-from-params
  "Returns an expression that builds the hashmap {:a b, :c d,...} given two args, [a, c,...] [b, d...]
  or returns {:a a, :b b,..} given arg [a,b...]"
  ([params] (bindings-expr-from-params params params))
  ([key-params, arg-params]
   `(hash-map ~@(interleave (map keyword key-params) arg-params))))

(defn macroexpand-keeping-metadata
  [expr]
  (let [expr-meta (meta expr)
        mexpr     (macroexpand expr)]
    (if expr-meta
      (with-meta mexpr expr-meta)
      mexpr)))

(defn nsymbols
  ([address n] (nsymbols address n 0))

  ([address n start]
   (let [base (str "p|" (clojure.string/join "-" (:point address)))]
     (map #(symbol (str base "|" %)) (range start n)))))

(defn throw-partition-error
  ([name expr] (throw-partition-error name expr nil))
  ([name expr msg & args]
   (let [loc        (ifit (:line (meta expr)) (format " (line %s)" it) "")
         extra-info (if msg (str ". " (apply format msg args)) "")]
     (throw (ex-info (format "%s%s%s: %s" name loc extra-info expr)
              (assoc (meta expr)
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

