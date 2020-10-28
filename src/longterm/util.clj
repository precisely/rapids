(ns longterm.util
  (:require [clojure.java.io :as io])
  (:import (java.util UUID Properties)
           (clojure.lang Namespace)))

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

(defmacro range-case [target & cases]
  "Compare the target against a set of ranges or constant values and return
   the first one that matches. If none match, and there exists a case with the
   value :else, return that target. Each range consists of a vector containing
   one of the following patterns:
     [upper-bound]                 if this is the first pattern, match any
                                   target <= upper-bound
                                   otherwise, match any target <= previous
                                   upper-bound and <= upper-bound
     [< upper-bound]               if this is the first pattern, match any
                                   target < upper-bound
                                   otherwise, match any target <= previous
                                   upper-bound and < upper-bound
     [lower-bound upper-bound]     match any target where lower-bound <= target
                                   and target <= upper-bound
     [< lower-bound upper-bound]   match any target where lower-bound < target
                                   and target <= upper-bound
     [lower-bound < upper-bound]   match any target where lower-bound <= target
                                   and target < upper-bound
     [< lower-bound < upper-bound] match any target where lower-bound < target
                                   and target < upper-bound
   Example:
     (range-case target
                 [0 < 1] :strongly-disagree
                 [< 2]     :disagree
                 [< 3]     :neutral
                 [< 4]     :agree
                 [5]       :strongly-agree
                 42          :the-answer
                 :else       :do-not-care)
   expands to
     (cond
       (and (<= 0 target) (< target 1)) :strongly-disagree
       (and (<= 1 target) (< target 2)) :disagree
       (and (<= 2 target) (< target 3)) :neutral
       (and (<= 3 target) (< target 4)) :agree
       (<= 4 target 5) :strongly-agree
       (= target 42) :the-answer
       :else :do-not-care)
    Test cases:
      (use '[clojure.test :only (deftest is run-tests)])
      (deftest unit-tests
        (letfn [(test-range-case [target]
                                 (range-case target
                                             [0 < 1] :strongly-disagree
                                             [< 2]   :disagree
                                             [< 3]   :neutral
                                             [< 4]   :agree
                                             [5]     :strongly-agree
                                             42      :the-answer
                                             :else   :do-not-care))]
      (is (= (test-range-case 0) :strongly-disagree))
      (is (= (test-range-case 0.5) :strongly-disagree))
      (is (= (test-range-case 1) :disagree))
      (is (= (test-range-case 1.5) :disagree))
      (is (= (test-range-case 2) :neutral))
      (is (= (test-range-case 2.5) :neutral))
      (is (= (test-range-case 3) :agree))
      (is (= (test-range-case 3.5) :agree))
      (is (= (test-range-case 4) :strongly-agree))
      (is (= (test-range-case 4.5) :strongly-agree))
      (is (= (test-range-case 5) :strongly-agree))
      (is (= (test-range-case 42) :the-answer))
      (is (= (test-range-case -1) :do-not-care))))
    (run-tests)"
  (if (odd? (count cases))
    (throw (IllegalArgumentException. (str "no matching clause: "
                                        (first cases))))
    `(cond
       ~@(loop [cases cases ret [] previous-upper-bound nil]
           (cond
             (empty? cases)
             ret

             (= :else (first cases))
             (recur (drop 2 cases) (conj ret :else (second cases)) nil)

             (vector? (first cases))
             (let [condition (first cases)
                   clause    (second cases)

                   [case-expr prev-upper-bound]
                   (let [length (count condition)]
                     (cond
                       (= length 1)
                       (let [upper-bound (first condition)]
                         [(if previous-upper-bound
                            `(and (<= ~previous-upper-bound ~target)
                               (<= ~target ~upper-bound))
                            `(<= ~target ~upper-bound))
                          upper-bound])

                       (= length 2)
                       (if (= '< (first condition))
                         (let [[_ upper-bound] condition]
                           [(if previous-upper-bound
                              `(and (<= ~previous-upper-bound ~target)
                                 (< ~target ~upper-bound))
                              `(< ~target ~upper-bound))
                            upper-bound])
                         (let [[lower-bound upper-bound] condition]
                           [`(and (<= ~lower-bound ~target)
                               (<= ~target ~upper-bound))
                            upper-bound]))

                       (= length 3)
                       (cond
                         (= '< (first condition))
                         (let [[_ lower-bound upper-bound] condition]
                           [`(and (< ~lower-bound ~target)
                               (<= ~target ~upper-bound))
                            upper-bound])

                         (= '< (second condition))
                         (let [[lower-bound _ upper-bound] condition]
                           [`(and (<= ~lower-bound ~target)
                               (< ~target ~upper-bound))
                            upper-bound])

                         :else
                         (throw (IllegalArgumentException. (str "unknown pattern: "
                                                             condition))))

                       (and (= length 4)
                         (= '< (first condition))
                         (= '< (nth condition 3)))
                       (let [[_ lower-bound _ upper-bound] condition]
                         [`(and (< ~lower-bound ~target) (< ~target ~upper-bound))
                          upper-bound])

                       :else
                       (throw (IllegalArgumentException. (str "unknown pattern: "
                                                           condition)))))]
               (recur (drop 2 cases)
                 (conj ret case-expr clause)
                 prev-upper-bound))

             :else
             (let [[condition clause]
                   `[(= ~target ~(first cases)) ~(second cases)]]
               (recur (drop 2 cases) (conj ret condition clause) nil)))))))

(defn new-uuid []
  (str (UUID/randomUUID)))

(defn suspend-op? [op]
  (some #(= op %) '[suspend! longterm/suspend! longterm.runloop/suspend! longterm.runloop/internal-suspend!]))

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
  (with-open [pom-properties-reader (io/reader (io/resource "META-INF/maven/longterm/longterm/pom.properties"))]
    (doto (Properties.)
      (.load pom-properties-reader))))

(defmacro ifit
  ([test then] `(ifit ~test ~then nil))
  ([test then else]
   `(let [~'it ~test] (if ~'it ~then ~else))))