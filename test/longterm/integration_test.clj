(ns longterm.integration_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all]))

(def ^:dynamic *log* (atom []))
(defn clear!
  []
  (reset! *log* []))

(defn log!
  [val]
  (reset! *log* (conj @*log* val)))

(defmacro is-log
  [expected]
  `(is (= ~expected @*log*)))

(deflow suspending-flow
  []
  (log! :before-suspend)
  (suspend :test-event)
  (log! :after-suspend))

(deftest ^:unit SystemTest
  (testing "Start and suspend"
    (clear!)
    (let [run (start-run! `(suspending-flow))]
      (is-log [:before-suspend])

      (testing "handling event"
        (let [pe (process-event! {:event-id :test-event :run-id (:id run)})]
          (println ">>> process-event returns:" pe)
          (is-log [:before-suspend :after-suspend]))))))


#_(let* [] (clojure.core/push-thread-bindings
           (clojure.core/hash-map (var longterm.runner/*run*)
             (longterm.runstore/create-run! :running)))
         (try (clojure.core/let [run__4080__auto__ longterm.runner/*run*]
                (clojure.core/let [result__4069__auto__
                                   (do (longterm.runner/resume-run! run__4080__auto__
                                         (clojure.core/fn [longterm.runner/_]
                                           (longterm.flow/start :foo)))
                                       run__4080__auto__) state__4070__auto__
                                   (if (longterm.runner/suspend-signal? result__4069__auto__)
                                     :suspended
                                     :complete)]
                  (longterm.runstore/save-run!
                    (clojure.core/assoc run__4080__auto__ :state state__4070__auto__ :result result__4069__auto__))))
              (finally (clojure.core/pop-thread-bindings))))
