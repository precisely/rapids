(ns longterm.runner
  (:require
    [longterm.runstore :as rs :refer [run-state?]]
    [longterm.flow :as flow]
    [longterm.util :refer :all])
  (:import (longterm.address Address)
           (longterm.runstore Run)))

(declare start-run! process-event! resume-run!)
(declare resume-at! next-continuation!)
(declare suspend-signal?)

(def ^:dynamic *run*)

(defmacro with-run!
  "Takes a run-id, and obtains a run, transitions it to running state and executes forms in body,
  then saves the run in :suspended or :complete state.

  The final form of body returns a Suspend record to put the run in :suspended state. If it returns
  a value, the run state will change to :complete, and the value will be stored in the
  Run's `result` field.

  Returns:
    run - Run instance in :suspended or :complete state"
  ([[run-form] & body]
   `(binding [*run* ~run-form]
      (let [value# (do ~@body)
            current-stack# (:stack *run*)
            [state#, stack#, result#] (if (suspend-signal? value#)
                                        [:suspended, (cons value#, current-stack#), nil]
                                        [:complete, current-stack#, value#])]
        (set! *run* (assoc *run* :state state# :result result# :stack stack#))
        (rs/save-run! *run*)))))

(defn start-run!
  "Starts a run with the flow and given arguments.
  Returns the Run in :suspended or :complete state."
  [flow & args]
  {:pre  [(refers-to? flow/flow? flow)]
   :post [(run-state? % :suspended :complete)]}
  (with-run! [(rs/create-run! :running)]
             (resume-run! (fn [_]
                            (apply flow/start flow args)))))

(defn resume-run!
  "Evaluates a par-bound continuation, popping the stack and passing the result to the next
   continuation until either a continuation returns SUSPEND or the stack is empty.

   This function destructively modifies the run, but DOES NOT save it. It should
   be wrapped inside a with-run! block."
  ([continuation] (resume-run! continuation nil))

  ([continuation result]
   {:pre [(run-state? *run* :running)
          (fn? continuation)]}
   (let [next-result (continuation result)]
     (if (suspend-signal? next-result)
       next-result
       (let [next-continuation (next-continuation!)]
         (if next-continuation                              ;
           (recur next-continuation next-result)
           next-result))))))                                ; final result returned

(defn process-event!
  "Processes an external event, finds the associated run and calls the continuation at the
  top of the stack, passing the event data.

  Args:
    event - a map of the form {:event-id event-id :run-id: run-id :data value}

  Returns:
    run - in :suspended or :complete state
  "
  [event]
  {:pre  [(-> event :run-id nil? not)
          (-> event :event-id nil? not)]
   :post [(run-state? % :suspended :complete)]}
  (let [run-id (:run-id event)
        event-id (:event-id event)
        result (:data event)]
    (with-run! [(rs/unsuspend-run! run-id)]
               (resume-run! (next-continuation! event-id) result))))

(defrecord StackFrame [address bindings result-key])

(defn- continuation-from-frame [frame]
  {:pre [(instance? StackFrame frame)]}
  (let [address (:address frame)
        result-key (:result-key frame)
        bindings (:bindings frame)]
    (fn [result]
      (let [bindings-with-result (if result-key
                                   (assoc bindings
                                     result-key result)
                                   bindings)]
        (flow/continue address bindings-with-result)))))

(defn next-continuation!
  "This function destructively updates the current run (popping the stack), so should
  only be called inside a dynamical context established by (with-run...).

  Returns:
   function (fn [result] ...) which returns result to the continuation at the top of the stack
   or nil if stack is empty"
  ([] (next-continuation! nil))

  ([event-id]
   (let [[frame & rest-frames] (:stack *run*)]
     (when frame
       (set! *run* (assoc *run* :stack rest-frames))
       (cond
         (nil? event-id) (continuation-from-frame frame)
         :else (do
                 (if-not (suspend-signal? frame)
                   (throw (Exception. (format "Invalid stack for run %s; expecting Suspend but received %s" frame))))
                 (recur nil)))))))

(defmacro resume-at
  "Generates code that continues execution at address after flow-form is complete.
  address - names the continuation
  params - list of parameters needed by the continuation
  result-key - the key to which the value of form will be bound in the continuation
  body - expression which invokes a flow

  Returns:
  value of body
  "
  ([[address params result-key] & body]
   (:pre [(instance? Address address)
          (vector? params)
          (or (nil? result-key) (symbol? result-key))])
   (let [hashmap-args (apply concat (map #(vec `('~% ~%)) params))]
     `(let [bindings# (hash-map ~@hashmap-args)
            new-frame# (StackFrame. ~address bindings# ~result-key)]
        (set! *run* (assoc *run* :stack (cons new-frame# (:stack *run*))))
        ~@body))))

;;
;; SUSPEND
;;

(defrecord Suspend [event-id expiry])

(defn suspend-signal? [x] (instance? Suspend x))

(defn suspend!
  "Used within deflow body to suspend execution until event with id=event-id is received."
  ([event-id] (suspend! event-id nil))
  ([event-id expiry]
   {:pre [(not (nil? event-id))]}
   (if-not (and (bound? #'*run*) (instance? Run *run*))
     (throw (Exception. (format "Invalid run context while evaluating (suspend! %s %s)"
                                event-id expiry))))
   (Suspend. event-id expiry)))