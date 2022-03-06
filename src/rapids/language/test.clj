;;;;
;;;; Clojure-test helpers which make it easy to test branching logic
;;;;
;;;; E.g.,
;;;;
;;;; (deftest WelcomeTest
;;;;   (branch "welcome" [run (start! welcome)]
;;;;     (keys-match run
;;;;       :state :suspended
;;;;       :output ["welcome. Do You want to continue?" _])
;;;;
;;;;     (branch "wants to continue"
;;;;       [run (continue! (:id run) :input "yes")]
;;;;       (keys-match run
;;;;         :state :suspended 
;;;;         :output ["great!... let's continue"]))
;;;;
;;;;     (branch "doesn't want to continue"
;;;;       [run (continue! (:id run) :input "no")]
;;;;       (keys-match run
;;;;         :state :complete)))

(ns rapids.language.test
  (:require [clojure.core.match :refer [match]]
            [clojure.core.match.regex]
            [clojure.test :refer :all]
            [rapids :refer :all]
            [rapids.implementations.in-memory-storage :as imrs]
            [rapids.storage.core :as s]))

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

(defmacro branch [bindings string & forms]
  ;; TODO: put the string (name) before bindings once Cursive supports custom macro argument resolution
  "Creates a series of tests where alternative steps can be tested. Each route from the root branch
  to the leaf branches is wrapped in a test environment.

  (branch [root 1]
    \"root\"
    ...
    (branch [a 1]
      \"A\"
      ...
      (branch [b 2]
        \"B\"
         ...)
      (branch [c 3]
        \"C\"
         ...))
    (branch [d 4]
      \"D\"
      ...
      (branch [e 5]
        \"E\"
        ...)))

  Executes 3 test environments:
   - root, A, B
   - root, A, C
   - root, D, E

  By expanding to:
  (do
    (with-test-env
      (testing \"root\"
        (let [root 1]
          ...
          (testing \"A\"
            (let [a 1]
              ...
              (testing \"B\"
                (let [b 2]
                  ...)))))
    (with-test-env
      (testing \"root\"
        (let [root 1]
          ...
          (testing \"A\"
            (let [a 1]
              ...
              (testing \"C\"
                (let [c 3]
                  ...)))))
    (with-test-env
      (testing \"root\"
        (let [root 1]
          ...
          (testing \"D\"
            (let [d 4]
              ...
              (testing \"E\"
                (let [e 5]
                  ...)))))"
  (letfn [(branch? [x]
            (and (seq? x) (-> x first #{`branch 'branch})))
          (expand-branch [[op bindings doc & forms]]
            {:pre [(#{'branch `branch} op)
                   (string? doc)
                   (and (vector? bindings) (-> bindings count even?))]}
            (loop [non-branch-elts []
                   results         []
                   children        forms]
              (if (empty? children)
                (if (empty? results)
                  `((testing ~doc (let ~bindings ~@non-branch-elts)))
                  (map (fn [result] `(testing ~doc (let ~bindings ~result))) results))
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
      :output [\"great!... let's continue\"]))

    is equivalent to:

    (let [r run]
      (is (match [(:state r)]
            [:running] true
            [_] false))
      (is (match [(:output r)]
            [[\"great! ... let's continue\"]] true
            [_] false)))"
  [obj & key-matches]
  (let [ovar (gensym "obj")
        body (loop [result []
                    [k m & rest-kms] key-matches]
               (let [new-result (conj result,
                                      (with-meta
                                        `(let [kval# (~k ~ovar)]
                                           (is (match [kval#]    ; key of object
                                                 [~m] true  ; the match pattern
                                                 [~'_] false) (str "Key match " '~k " failed.  "
                                                                   "Pattern " '~m " does not match " kval#)))
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
