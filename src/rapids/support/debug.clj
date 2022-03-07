;; debugging utilities
(ns rapids.support.debug
  (:require [clojure.pprint :as pprint]))

(defn print-result
  ([& args] (apply println args) (last args)))

(defn macroexpand-n [f n]
  (if (zero? n) f (macroexpand-n (macroexpand-1 f) (dec n))))

(defn macroexpand-pprint [form]
  (binding [pprint/*print-suppress-namespaces* true]
    (pprint/pprint (macroexpand form))))

(defn macroexpand-pprint-1 [form]
  (binding [pprint/*print-suppress-namespaces* true]
    (pprint/pprint (macroexpand-1 form))))

(defn macroexpand-pprint-1 [form n]
  (binding [pprint/*print-suppress-namespaces* true]
    (pprint/pprint (macroexpand-n form n))))