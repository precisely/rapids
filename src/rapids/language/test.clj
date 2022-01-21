;;;;
;;;; Clojure-test helpers which make it easy to test branching logic
;;;;
;;;; E.g.,
;;;;
;;;; (deftest WelcomeTest
;;;;   (branch "welcome" [run (start! welcome)]
;;;;     (keys-match run
;;;;       :state :suspended
;;;;       :response ["welcome. Do You want to continue?" _])
;;;;
;;;;     (branch "wants to continue"
;;;;       [run (continue! (:id run) :data "yes")]
;;;;       (keys-match run
;;;;         :state :suspended 
;;;;         :response ["great!... let's continue"]))
;;;;
;;;;     (branch "doesn't want to continue"
;;;;       [run (continue! (:id run) :data "no")]
;;;;       (keys-match run
;;;;         :state :complete)))

(ns rapids.language.test
  (:require [rapids :refer :all]
            [clojure.test :refer :all]
            [clojure.tools.macro :refer [macrolet]]
            [rapids.storage.core :as s]
            [rapids.implementations.in-memory-storage :as imrs]
            [clojure.core.match :refer [match]]))

(declare branch)

(defmacro with-test-storage [& body]
  `(s/with-storage (imrs/->in-memory-storage)
     ~@body))

(defmacro with-test-env
  "Provides a storage and an active connection"
  [& body]
  {:pre [(not (vector? (first body)))]}
  `(with-test-storage
     (s/ensure-cached-connection
       ~@body)))

(defmacro branch [string & forms]
  "Creates a series of tests where alternative steps can be tested. Each route from the root branch
  to the leaf branches is wrapped in a test environment.

  (branch \"root\"
    ...
    (branch \"1\"
      ...
      (branch \"a\"
         ...)
      (branch \"b\"
         ...))
    (branch \"2\"
      ...
      (branch \"c\"
        ...)))

  Expands to 3 test sequences:
  (do
    (with-test-env
      (testing \"root\"
        ...
        (testing \"1\"
          ...
          (testing \"a\"
           ....))))
    (with-test-env
      (testing \"root\"
        ...
        (testing \"1\"
          ...
          (testing \"b\"
           ....))))
    (with-test-env
      (testing \"root\"
        ...
        (testing \"2\"
          ...
          (testing \"c\"
           ...)))))"
  (letfn [(branch? [x]
            (and (list? x) (-> x first #{`branch 'branch})))
          (expand-branch [[op name & forms]]
            {:pre [(#{'branch `branch} op) (string? name)]}
            (loop [non-branch-elts []
                   results         []
                   children        forms]
              (if (empty? children)
                (if (empty? results)
                  `((clojure.test/testing ~name ~@non-branch-elts))
                  (map #(list* `clojure.test/testing name %) results))
                (let [[child & remaining-children] children]
                  (if (branch? child)
                    (let [expanded-branch (expand-branch child)]
                      (recur non-branch-elts
                             (concat results (map #(conj non-branch-elts %) expanded-branch))
                             remaining-children))
                    (recur (conj non-branch-elts child)
                           results
                           remaining-children))))))]
    `(do ~@(map #(list `with-test-env %) (expand-branch &form)))))

(defmacro keys-match
  "Checks that the keys of obj match a pattern. See clojure.core.match for documentation
  on how to use the matching syntax.

  Arguments: obj - object to be matched
             key-matches - key-match pairs
  Example:
    (keys-match run
      :state :running
      :response [\"great!... let's continue\"])

    is equivalent to:

    (let [r run]
      (is (match [(:state r)]
            [:running] true
            [_] false))
      (is (match [(:response r)]
            [[\"great! ... let's continue\"]] true
            [_] false)))"
  [obj & key-matches]
  (let [ovar (gensym "obj")
        body (loop [result []
                    [k m & rest-kms] key-matches]
               (let [new-result (conj result,
                                      (with-meta
                                        `(is (match [(~k ~ovar)]
                                               [~m] true
                                               [~'_] false))
                                        (meta m)))]
                 (if (empty? rest-kms)
                   new-result
                   (recur new-result, rest-kms))))]
    (with-meta `(let [~ovar ~obj]
                  ~@body) (meta &form))))

(defn flush-cache!
  "For ease of testing - simulates the end of a request by flushing the cache contents to the storage and clearing it"
  []
  (rapids.storage.cache/save-cache!)
  (set! rapids.storage.globals/*cache* {})
  (assert (empty? rapids.storage.globals/*cache*)))
