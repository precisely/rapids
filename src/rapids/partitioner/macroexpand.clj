(ns rapids.partitioner.macroexpand
  (:require [clojure.string :as str]))

(defn make-gensym-context [] {:counter    (atom 0)
                              :map        (atom {})
                              :exclusions #{}})

(def ^:dynamic *gensym-context* (make-gensym-context))

(defn tagged-gensym
  ([] (tagged-gensym ""))
  ([s] (symbol (str s (swap! (:counter *gensym-context*) inc) "__gensym__auto__"))))

(defn in-gensym-context? [] (bound? #'*gensym-context*))

(defmacro with-gensym-context
  "Establishes a fresh gensym context (generally used for each deflow)" [& body]
  `(binding [*gensym-context* (make-gensym-context)]
     ~@body))

(defn gensym-map [] (:map *gensym-context*))

(defn gensym-counter [] (:counter *gensym-context*))

(defn gensym-exclusions []
  {:post [(set? %)]}
  (:exclusions *gensym-context*))

(defn gensym-replacement? [o]
  (and (symbol? o) (->> o name (re-matches #"^\<\<\d+\>\>$"))))

(defn gensym-replacable?
  "Prevents symbols from being replaced. All symbols which resolve to vars and any specifically
  excluded symbols (e.g., lexical bindings) are excluded from gensym replacement.

  (Gensyms are detected by looking for numbers or __auto__ at the end of the symbol name.)"
  [o]
  (not (or (contains? (gensym-exclusions) o)
         (and (symbol o) (resolve o)))))

(defn stable-symbol
  "Given a symbol, returns a new unique symbol within a gensym context which is guaranteed to be
  the same across macroexpansions of the same code."
  ([] (stable-symbol (gensym)))
  ([old-sym]
   (or (get @(gensym-map) old-sym)
     (let [new-sym (symbol (str "<<" (swap! (gensym-counter) inc) ">>"))]
       (swap! (gensym-map) assoc old-sym new-sym)
       new-sym))))

;;(defn exclude-from-gensym-replacement [symbols]
;;  (let [symbols (cond
;;                  (symbol? symbols) #{symbols}
;;                  (sequential? symbols) (set symbols)
;;                  :else (assert false "Expecting symbol list or set in exclude-from-gensym-replacement"))]
;;    (swap! (gensym-exclusions) clojure.set/union symbols)))

(defmacro with-gensym-excluded-symbols
  "Prevents certain symbols from being replaced by gensym"
  [symbols & body]
  `(binding [*gensym-context* (update *gensym-context* :exclusions clojure.set/union (set ~symbols))]
     ~@body))

(defn replace-gensyms
  "Detects symbols ending with __auto__ in the body, replacing them with symbols generated using a counter that starts at 1"
  [body]
  {:pre [(in-gensym-context?)]}
  (letfn [(is-replacable? [o]
            (and
              (symbol? o)
              (gensym-replacable? o)
              (let [n (name o)]
                (or (str/ends-with? n "__auto__")
                  (re-find #"\d+$" n)))))
          (substitution [o]
            (if (is-replacable? o)
              (stable-symbol o)
              o))]
    (clojure.walk/prewalk substitution body)))

(defn- _partition-expander [f form]
  (let [form-meta (meta form)
        mexpr     (replace-gensyms (f form))]
    (if form-meta
      (if mexpr (with-meta mexpr form-meta))
      mexpr)))

(defn partition-macroexpand
  "Like macroexpand, but preserves metadata and replaces gensyms"
  [form]
  (_partition-expander macroexpand form))

(defn partition-macroexpand-1
  "Like macroexpand-1, but preserves metadata and replaces gensyms"
  [form]
  (_partition-expander macroexpand-1 form))
