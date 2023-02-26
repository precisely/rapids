(ns rapids.language.operators
  (:require [clojure.spec.alpha :as s]
            [rapids.objects.signals :as signals]
            [rapids.runtime.run-loop :as rl]
            [rapids.runtime.runlet :as rt]
            [taoensso.timbre :as log]
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
  (let [normalized-permit (if (keyword? permit)
                            (name permit) permit)]
    (if (not= normalized-permit permit)
      (log/warn (str "Keyword permit" permit " normalized to string. Please change this in your code.")))

    (signals/make-suspend-signal permit expires default)))

(defn output!
  "Adds an element to the current run response: returns nil"
  [& responses]
  (apply rt/add-responses! responses))

;;
;; wait-for
;;
(defn- make-wait-for-any-start-partition
  "Suspends the current run until the provided child-run completes."
  [name]
  (fn [{:keys [runs default expires]}]
    (let [[run i] (first (keep-indexed (fn [i r]
                                         (if (-> r :state (= :complete))
                                           [r i]))
                           runs))]
      (resume-at [(a/->address name 1) [runs] result]
        (if run
          [i (:result run)]
          (do
            (doall (map-indexed (fn [i run] (rt/attach-child-run! run i)) runs))
            (input! :permit (set (map :id runs))
              :expires expires
              :default [nil default])))))))

(defn- make-wait-for-any-final-partition [finalizer]
  (fn [{:keys [runs result]}]
    ;; remove the current run as a parent from all child runs
    (doall (map rt/detach-child-run! runs))
    (finalizer result)))

(defn- make-wait-for-flow [name entry-point finalizer]
  (->Flow name entry-point
    {[0] (make-wait-for-any-start-partition name)
     [1] (make-wait-for-any-final-partition finalizer)}))

(defn- start-take
  "Calls the first partition of a take flow"
  [name runs default expires]
  {:pre [(every? rt/run? runs) (distinct? (map :id runs))]}
  (flow/call-partition (a/->address name 0)
    {:runs runs :default default :expires expires}))

(def wait-for-any!
  (make-wait-for-flow `wait-for-any!
    (fn wait-for-any!
      ([runs] (wait-for-any! runs nil nil))
      ([runs default] (wait-for-any! runs default :immediately))
      ([runs default expires]
       {:pre [(every? rt/run? runs)]}
       (start-take `wait-for-any! runs default expires)))
    identity))

(def wait-for!
  (make-wait-for-flow
    `wait-for!
    (fn wait-for!
      ([run] (wait-for! run nil nil))
      ([run default] (wait-for! run default :immediately))
      ([run default expires]
       {:pre [(rt/run? run)]}
       (start-take `wait-for! [run] default expires)))
    second))

(defmacro
  ^{:arglists '([v & cases] [[v default? expires?] & cases])}
  wait-for-case!
  "Execute a block of code when one or more runs completes. This is a thin
  convenience wrapper around wait-for-any! Similar in spirit to a case statement.

  v     - a symbol which will be bound to the value or a vector [sym default? expires?]
  cases - r1 e1 r2 e2...
          Where ri is a symbol bound to a run
          and ei is an expression to be executed when the corresponding run returns

  Note: if default is provided, the form is guaranteed to return immediately, otherwise it may suspend
  (wait-for-case! v
    r1 (print \"Run 1 returned =>\" v)
    r2 (print \"Run 2 returned =>\" v))

  ;; take-case! with a default
  (wait-for-case! [v :foo]
    r1 (print \"Run 1 returned =>\" v)
    r2 (print \"Run 2 returned =>\" v)) ; returns :foo immediately if r1 and r2 are not complete

  ;; take-case! with a default and expiry
  (wait-for-case! [v :foo (-> 5 days from-now)]
    r1 (print \"Run 1 returned =>\" v)
    r2 (print \"Run 2 returned =>\" v)) ; returns :foo in 5 days if r1 and r2 do not complete"
  [v & cases]
  (let [[has-default? variable default expiry]
        (if (vector? v)
          `[(> (count v) 1) ~@v]
          [false v])
        [runs blocks] (rapids.support.util/reverse-interleave cases 2)]
    `(let [[index# ~variable] (wait-for-any! ~runs ~@(if has-default? `(~default ~(or expiry :immediately))))]
       (case index#
         ~@(interleave (range (count runs)) blocks)
         ;; take-any! returns [nil default] for default
         nil ~variable))))

;;
;; Shortcut operators
;;
(def ! rl/start!)
(alter-meta! #'! #(merge (meta #'rl/start!) %))

(def <* input!)
(alter-meta! #'<* #(merge (meta #'input!) %))

(def >* output!)
(alter-meta! #'>* #(merge (meta #'output!) %))
