;; debugging utilities
(ns rapids.support.debug
  (:require [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [rapids.objects.address :as a]
            [rapids.partitioner.partition-set :as pset]
            [rapids.objects.flow :as flow])
  (:import (clojure.lang MapEntry PersistentHashSet)))
(defn macroexpand-pprint [form]
  (binding [pprint/*print-suppress-namespaces* true]
    (pprint/pprint (macroexpand form))))

(defn print-result
  ([r] (println r) r)
  ([msg r] (println msg r) r)
  ([msg from to] (println msg from "=>" to) to))

(defn weird-symbol-replacer
  ([form] (weird-symbol-replacer form "_v" (atom {}) (atom 0)))
  ([form base symbol-dict counter]
   (letfn [(weird-symbol? [o]
             (and (symbol? o)
               (re-find #"[\|]|(\d\-\d)|(\d_\d)|(__)" (str o))))
           (new-symbol []
             (symbol (str base (swap! counter inc))))
           (nice-map [m]
             (zipmap (wsr (keys m)) (wsr (vals m))))
           (nice-symbol [s]
             (if (weird-symbol? s)
               (or (get @symbol-dict s)
                 (let [news (new-symbol)]
                    (swap! symbol-dict assoc s news)
                    news))
               (symbol (name s))))
           (nice-keyword [k]
             (let [s (symbol k)]
               (if (weird-symbol? s)
                 (keyword (nice-symbol s))
                 k)))
           (nice-record [r]
             (wsr (into {} r)))
           (wsr [x] (weird-symbol-replacer x base symbol-dict counter))]

     (walk/postwalk #(cond
                       (keyword? %) (nice-keyword %)
                       (symbol? %) (nice-symbol %)
                       (a/address? %)  (keyword "address" (str (nice-symbol (symbol (a/to-string %)))))
                       (pset/partition? %) {:d:partition (nice-record %)}
                       (flow/flow? %) {:d:flow (nice-record %)}
                       (map? %) (nice-map %)
                       (seq? %) (seq %)
                       (vector? %) (vec (map (fn [elt] (weird-symbol-replacer elt base symbol-dict counter))
                                          %))
                       (number? %) %
                       (string? %) %
                       (boolean? %) %
                       (nil? %) %
                       (set? %) (set (wsr (seq %)))
                       (map-entry? %) (print-result "map-entry" % (MapEntry. (wsr (key %)) (wsr (val %))))
                       :otherwise (print-result "unhandled type" (class %) %))
       form))))
