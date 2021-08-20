(ns rapids.util
  (:require [clojure.java.io :as io])
  (:import (java.util UUID Properties)
           (clojure.lang Namespace Cons Atom)))

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

(defmacro ifit
  "If with implicit or explicit binding of the test value:

  Usage:

  ;; implicit
  (if (find-object)
    (do-something-with-object it))

  ;; explicit
  (if [myobj (find-object)]
    (do-something-with-object myobj))

  ;; multiple bindings allowed
  (if [myobj (find-object) ;; only myobj is tested
       val  (foo)]  ;; val can be nil
    (do-something-with myobj val))"
  ([test then] `(ifit ~test ~then nil))
  ([test then else]
   (let [bindings (if (vector? test) test ['it test])]
    `(let ~bindings (if ~(first bindings) ~then ~else)))))

(defmacro bind-once [[k v] & body]
  `(letfn [(do-body# [] ~@body)]
     (if (bound? #'~k)
       (do-body#)
       (binding [~k ~v] (do-body#)))))

(defn unqualified-symbol? [o]
  (and (symbol? o) (not (qualified-symbol? o))))

(defn key-to-str [key]
  (if key (name key)))

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
