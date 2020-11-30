(ns longterm.partition-utils
  (:require [longterm.util :refer [ifit in? unqualified-symbol?]]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [intersection]]))

;;
;; HELPERS
;;
(defn bindings-expr-from-params
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
     (throw (Exception. (format "%s%s%s: %s" name loc extra-info expr))))))

(defn unqualified-symbols-in [form]
  "Returns a set of unqualified symbols appearing in form"
  (set (filter unqualified-symbol?
         (postwalk #(cond
                      (seq? %) (flatten %)
                      (map? %) (flatten (map identity %))
                      :else %) form))))

(defn params-used-by
  "Given a form and a vector of unqualified symbols, returns the symbols which appear in form"
  [form env-params]
  (let [form-bindings (unqualified-symbols-in form)]
    (vec (intersection form-bindings env-params))))

