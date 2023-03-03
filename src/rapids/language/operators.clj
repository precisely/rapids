(ns rapids.language.operators
  (:require [clojure.spec.alpha :as s]
            [rapids.objects.signals :as signals]
            [rapids.runtime.run-loop :as rl]
            [rapids.runtime.runlet :as rt]
            [rapids.support.util :refer [reverse-interleave]]
            [rapids.objects.address :as a]
            [rapids.partitioner.resume-at :refer [resume-at]]
            [rapids.objects.flow :refer [->Flow]]
            [rapids.objects.flow :as flow])
  (:import (java.time LocalDateTime)))

(s/def ::json (s/or
                :string string?
                :number number?
                :boolean boolean?
                :null nil?
                :map (s/map-of string? ::json)
                :array (s/and vector? (s/coll-of ::json))))
(s/def ::permit (s/or :keyword keyword? :json ::json :uuid uuid? :callable ifn?))
(defn ^:suspending input!
  [& {:keys [permit expires default]}]
  {:pre [(s/valid? (s/nilable ::permit) permit)
         (s/valid? (s/nilable #(instance? LocalDateTime %)) expires)]}
  (signals/make-suspend-signal permit expires default))

(defn output!
  "Adds an element to the current run output: returns nil"
  [& outputs]
  (apply rt/add-outputs! outputs))

;;
;; wait-for
;;
(defn- make-wait-for-any-start-partition
  "Suspends the current run until the provided child-run completes."
  [name]
  (fn [{:keys [runs default expires]}]
    (let [[run i] (first (keep (fn [[i r]]
                                 (if (-> r :state (= :complete))
                                   [r i]))
                           runs))]
      (resume-at [(a/->address name 1) [runs] result]
        (if run
          [i (:result run)]
          (if (= expires :immediately)
            [:default default]
            (do
              (doall (map (fn [[i run]] (rt/attach-waiting-run! run i)) runs))
              (input! :permit (set (map :id (vals runs)))
                :expires expires
                :default [:default default]))))))))

(defn- make-wait-for-any-final-partition [finalizer]
  (fn [{:keys [runs result]}]
    ;; remove the current run as a parent from all child runs
    (doall (map rt/detach-waiting-run! (vals runs)))
    (finalizer result)))

(defn- make-wait-for-flow [name entry-point finalizer]
  (->Flow name entry-point
    {[0] (make-wait-for-any-start-partition name)
     [1] (make-wait-for-any-final-partition finalizer)}))

(defn- start-wait
  "Calls the first partition of a take flow"
  [name runs default expires]
  (let [runs (cond
               (map? runs) runs
               (sequential? runs) (into {} (map-indexed (fn [i r] [i r]) runs))
               :default (throw (ex-info "runs argument must be a sequence of Run objects or a map with values being Run objects"
                                 {:operation name :runs runs})))
        _    (if-not (every? rt/run? (vals runs)) (throw (ex-info "Invalid input to wait operation. Expecting Run."
                                                           {:operation name, :runs runs})))]
    (flow/call-partition (a/->address name 0)
      {:runs runs :default default :expires expires})))

(def wait-for-any!
  "Waits for any of the given runs to complete, returning the run and its result.
  This operation can timeout immediately or at a given time, returning a default value.

  Args:
  run - a run
  default - optional, if no runs are not complete, returns this value immediately. Equivalent to `(if (= :complete run) default (wait-for! run))`
  expires - optional, causes default to be returned if no runs complete by this time

  Returns:
  [source, result] - where source is one of the runs or :default
  
  Usage:
  (wait-for! [r1 r2 r3] :my-default (-> 3 days from-now))"
  (make-wait-for-flow `wait-for-any!
    (fn wait-for-any!
      ([runs] (wait-for-any! runs nil nil))
      ([runs default] (wait-for-any! runs default :immediately))
      ([runs default expires]
       (start-wait `wait-for-any! runs default expires)))
    identity))

(def wait-for!
  "Waits for the given run to complete, returning its result. This operation can
  timeout immediately or at a given time, returning a default value.

  Args:
  run - a run
  default - optional, if run is not complete, returns this value immediately. Equivalent to `(if (= :complete run) default (wait-for! run))`
  expires - optional, causes default to be returned if run does not complete by this time

  Returns:
  the result of the run or default

  Usage:
  (wait-for! run :my-default (-> 3 days from-now))"
  (make-wait-for-flow
    `wait-for!
    (fn wait-for!
      ([run] (wait-for! run nil nil))
      ([run default] (wait-for! run default :immediately))
      ([run default expires]
       {:pre [(rt/run? run)]}
       (start-wait `wait-for! [run] default expires)))
    second))

(defmacro ^{:arglists '([[resultvar & {:keys [default expires break]}] & cases])}
  wait-cases!
  "Waits for runs to complete, executing an expression associated with each run.
  Returns a results map, a map from runs which returned values to values returned
  by associated case clauses. Note: if the raw run result is needed, it can be
  retrieved from the run using `(:result run)` as per normal.

  Args:
  result - symbol which the run result will be bound to when executing each case
  expires - nil (never), a time in the future or `:immediately`
  default - the default value. If provided and expires is assumed to be :immediately, unless explicitly provided
  break - predicate taking the results map and returning true if the loop should terminate.
          By default, the loop terminates when all runs have returned results.
          To only wait on one run, pass a fn like #(= (count %) 1)

  Usage:
  (wait-cases! [result
                :default :foo
                :expires (-> 3 days from-now)
                :break #(= (count %) 5)] ; stop after 5 runs have completed
    run1 (list :foo result)
    run2 (list :bar result)
    ...
    :default (list :expired! result)) ; handle expiry or exit out if no runs are complete
                                      ; result will be :foo here (since :default was provided)
    =>
  ;; e.g., if only runs 2 and 4 completed within 3 days:
  {run2 (:foo run1-result)
   run4 (:baz run4-result)
   :default (:expired! :foo)} ; the presence of the :default key means it expired"
  [ctrl & cases]
  (let [[v & {:keys [expires break]}] (if (vector? ctrl) ctrl [ctrl])
        input-map (apply hash-map cases)
        run-map   (dissoc (into {} (map (fn [k] [`'~k k]) (keys input-map))) :default)
        case-map  (into {} (map (fn [[k case]] [`'~k `(rapids/flow [~v] ~case)]) input-map))
        default?  (if (contains? input-map :default) :default)
        _         (comment [runs cases] (reverse-interleave cases 2)
                    case-map (into {} (map-indexed (fn [i, case] [i, `(rapids/flow [~v] ~case)]) cases))
                    runs-map (into {} (remove nil? (map-indexed (fn [i, run] (if (rt/run? run) [i, run])) runs))))]
    `(let [expires#    ~expires
           expires#    (if (or ~default? expires#)
                         (or expires# :immediately))
           case-map#   ~case-map ; {0 (fn [v] user-code), 1 (fn [v] user-code)...}
           run-map#    (dissoc ~run-map :default)
           break-test# (or ~break (constantly false))] ; returns true when we should terminate
       (assert (ifn? break-test#))
       (loop [results# {}
              runs#    run-map#] ; {'run1 run1, 'run2 run2...}
         (let [[key# result#] (wait-for-any! runs# nil expires#) ; note: default is computed in the case, so always nil here
               runs#        (dissoc runs# key#)
               case-flow#   (get case-map# key#)
               case-result# (if case-flow# (rapids/fcall case-flow# result#))
               results#     (assoc results# key# case-result#)
               break?#      (or (empty? runs#) (= :default key#) (break-test# results#))]
           (if break?#
             ;; replace result keys with runs
             (into {} (map (fn [[k# result#]] [(get run-map# k# :default) result#]) results#))
             (recur results# runs#)))))))

;;
;; Shortcut operators
;;
(def ! rl/start!)
(alter-meta! #'! #(merge (meta #'rl/start!) %))

(def <* input!)
(alter-meta! #'<* #(merge (meta #'input!) %))

(def >* output!)
(alter-meta! #'>* #(merge (meta #'output!) %))
