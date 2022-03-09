(ns rapids.partitioner.partition-macroexpand-test
  (:require [clojure.test :refer :all]
            [rapids.partitioner.macroexpand :refer :all]))

(defmacro test-macro []
  (let [s (gensym)]
    `(list ~s)))

(def var123 :foo)

(deftest ^:unit partition-macroexpand-test
  (testing "partition macroexpander"
    (testing "macroexpanding autosyms results in non-equal lists "
      (is (not=
            (macroexpand `(a#))
            (macroexpand `(a#))))
      (is (=
            (with-gensym-context (partition-macroexpand `(a#)))
            (with-gensym-context (partition-macroexpand `(a#))))))

    (testing "macroexpanding gensyms results in non-equal lists "
      (is (not=
            (with-gensym-context (macroexpand `(test-macro)))
            (with-gensym-context (macroexpand `(test-macro)))))
      (is (=
            (with-gensym-context (partition-macroexpand `(test-macro)))
            (with-gensym-context (partition-macroexpand `(test-macro))))))

    (testing "macroexpanding the same macro twice ought to produce different variables"
      (with-gensym-context
        (is (not=
              (partition-macroexpand `(test-macro))
              (partition-macroexpand `(test-macro))))))
    (testing "macroexpanding clojure core macros also results in identical code"
      (is (not=
            (macroexpand `(doseq [a 1] a))
            (macroexpand `(doseq [a 1] a))))
      (is (=
            (with-gensym-context (partition-macroexpand `(doseq [a 1] a)))
            (with-gensym-context (partition-macroexpand `(doseq [a 1] a))))))
    (testing "metadata is preserved"
      (let [expr (with-meta '(and a b) {:column 123 :line 456})]
        (is (= (meta (with-gensym-context (partition-macroexpand expr)))
              {:column 123 :line 456}))))

    (testing "expanding clojure.core macros works as expected"
      (is (=
            (with-gensym-context (partition-macroexpand '(doseq [a '(1 2 3)] (println a))))
            (with-gensym-context (partition-macroexpand '(doseq [a '(1 2 3)] (println a)))))))

    (testing "gensym-replacement? detects our gensym-replacement symbols"
      (is (gensym-replacement? '<<123>>))
      (is (not (gensym-replacement? '<<foo>>)))
      (is (not (gensym-replacement? '<123>))))

    (testing "with-gensym-excluded-symbols should prevent symbols from being replaced"
      (is (not= 'g123 (with-gensym-context (partition-macroexpand 'var123))))
      (is (= 'g123 (with-gensym-context
                     (with-gensym-excluded-symbols '[g123]
                       (partition-macroexpand 'g123))))))

    (testing "excludes global defs from gensym replacement in macroexpand"
      (is (= var123 :foo))
      (is (not (nil? (resolve `var123))))
      (is (= `var123 (with-gensym-context (partition-macroexpand `var123)))))))

