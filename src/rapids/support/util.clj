(ns rapids.support.util
  (:require [clojure.java.io :as io])
  (:import (clojure.lang Atom Cons Namespace)
           (java.util Properties UUID)))

(defn refers-to?
  "Dereferences a symbol or var and applies pred to the referenced value"
  [pred val]
  (cond
    (pred val) true
    (symbol? val) (recur pred (ns-resolve *ns* val))
    (var? val) (recur pred (var-get val))
    :else false))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn new-uuid []
  (UUID/randomUUID))

(defn reverse-interleave
  "Reverses interleave of sequence s into n lists

  (reverse-interleave '[:a 1 :b 2 :c 3] 2) => [[:a :b :c] [1 2 3]]"
  [s n]
  (if (empty? s) [] (apply map vector (partition-all n s))))

(defn in?
  "True if array contains val. Have to use this because Clojure contains?
  checks for existence of keys, not values."
  [array val]
  (some #(= % val) array))

(defn qualify-symbol
  ([s] (qualify-symbol s *ns*))

  ([s ns]
   {:pre  [(symbol? s) (instance? Namespace ns)]
    :post [(qualified-symbol? %)]}
   (symbol (str (.getName ns)) (str (.getName s)))))

(defn linked-list?
  "Unlike the confusingly named `list?` this actually returns true for all lists; i.e., including things constructed with cons"
  [o]
  (or (list? o) (instance? Cons o)))

(defn unqualified-symbol? [o]
  (and (symbol? o) (not (qualified-symbol? o))))

(defn sausage-to-snake
  "Converts a sausage-style-keyword to a snake_style_keyword"
  [k]
  (keyword (clojure.string/replace (name k) "-" "_")))

(defn snake-to-sausage
  "Converts a :snake_style_keyword to a :sausage-style-keyword "
  [k]
  (keyword (clojure.string/replace (name k) "_" "-")))

(defn atom? [o] (instance? Atom o))

(defn contains-some? [m & ks]
  (some #(contains? m %) ks))

(defmacro setf!
  "Set the binding of var to the result of applying f to var with optional args.

  Similar behavior to swap!, but for bindings instead of atoms."
  [var f & args]
  `(set! ~var (~f ~var ~@args)))

(defn walk-with-meta
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall."

  {:added "1.1"}
  [inner outer form]
  (cond
    (list? form) (let [m (meta form)] (outer (with-meta (apply list (map inner form)) m)))
    (instance? clojure.lang.IMapEntry form)
    (outer (clojure.lang.MapEntry/create (inner (key form)) (inner (val form))))
    (seq? form) (outer (doall (map inner form)))
    (instance? clojure.lang.IRecord form)
    (outer (reduce (fn [r x] (conj r (inner x))) form form))
    (coll? form) (outer (into (empty form) (map inner form)))
    :else (outer form)))

(defn prewalk-with-meta
  "Like postwalk, but does pre-order traversal."
  {:added "1.1"}
  [f form]
  (walk-with-meta (partial prewalk-with-meta f) identity (f form)))

(defn postwalk-with-meta
  "Performs a depth-first, post-order traversal of form.  Calls f on
  each sub-form, uses f's return value in place of the original.
  Recognizes all Clojure data structures. Consumes seqs as with doall."
  {:added "1.1"}
  [f form]
  (walk-with-meta (partial postwalk-with-meta f) f form))
