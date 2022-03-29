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

(defn reverse-interleave [s n]
  "Reverses interleave of sequence s into n lists"
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

(defn get-project-info []
  "Gets the MAVEN groupId, artifactId, version and the version (the git hash)"
  (with-open [pom-properties-reader (io/reader (io/resource "META-INF/maven/rapids/rapids/pom.properties"))]
    (doto (Properties.)
      (.load pom-properties-reader))))

(defn linked-list?
  "Unlike the confusingly named `list?` this actually returns true for all lists; i.e., including things constructed with cons"
  [o]
  (or (list? o) (instance? Cons o)))

(defn unqualified-symbol? [o]
  (and (symbol? o) (not (qualified-symbol? o))))

(defn sausage-to-snake
  "Converts a :sausage-style-keyword to a :snake_style_keyword"
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

(defn take-to-first
  "Returns a lazy sequence of successive items from coll up to
  and including the point at which it (pred item) returns true.
  pred must be free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if-not (pred (first s))

        (cons (first s) (take-to-first pred (rest s)))
        (list (first s))))))

(defn partition-when
  "Applies f to each value in coll, splitting it each time f returns
  true. Returns a lazy seq of lazy seqs."
  [f coll]
  (when-let [s (seq coll)]
    (lazy-seq
      (let [run (take-to-first f s)
            res (drop (count run) s)]
        (cons run (partition-when f res))))))

(defn maprest
  ([f coll] (maprest f coll []))
  ([f coll result]
   (let [rcoll (rest coll)]
     (if (empty? rcoll) result
       (maprest f rcoll (conj result (f rcoll)))))))

(defn segregate
  "Takes a collection of n-tuples and returns n collections, where each collection represents the ith element of the n-tuple,
  removing items for which predicate p is true. If not provided, p is nil?

  (segregate 3 '([a nil 1] [nil b 2] [c nil 3] [nil d nil]))
  => ((a c) (b d) (1 2 3))"
  ([n coll] (segregate n identity coll))
  ([n p coll]
   (let [splitter (apply juxt (map (fn [i] #(map (fn [v] (nth v i)) %)) (range n)))]
     (map #(remove p %)
       (splitter coll)))))