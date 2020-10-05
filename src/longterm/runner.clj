(ns longterm.runner
  (:require
    [longterm.runstore :as rs :refer [run-state?]]
    [longterm.flow :as flow]
    [longterm.util :refer :all])
  (:import (longterm.address Address)
           (longterm.runstore Run)))

(declare start-run! process-event! resume-run!)
(declare resume-at! next-continuation!)

(def ^:dynamic *run*)
(def ^:const SUSPEND ::SUSPEND)
(defn suspend-signal? [x] (= x SUSPEND))

(defmacro with-run!
  "Takes a run-id, and obtains a run, transitions it to running state and executes forms in body,
  then saves the run in :suspended or :complete state.

  The final form of body returns SUSPEND to put the run in :suspended state. If it returns
  a value, the run state will change to :complete, and the value will be stored in the
  Run's `result` field.

  Returns:
    run - in :suspended or :complete state"
  ([[run-form] & body]
   `(binding [*run* ~run-form]
      (let [result# (do ~@body)
            state#  (if (suspend-signal? result#) :suspended :complete)]
        (set! *run* (assoc *run* :state state# :result result#))
        (println "with-run! saving run" *run*)
        (rs/save-run! *run*)))))

(defn start-run!
  "Begins a new run"
  [flow-form]
  {:pre  [(list? flow-form)
          (refers-to? flow/flow? (first flow-form))]
   :post [(run-state? % :suspended :complete)]}
  (let [[flow & args] flow-form]
    (with-run! [(rs/create-run! :running)]
      (resume-run! (fn [_]
                     (apply flow/start flow args))))))

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
         (if next-continuation          ;
           (recur next-continuation next-result)
           next-result))))))            ; final result returned

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
  (let [run-id   (:run-id event)
        event-id (:event-id event)
        result   (:data event)]
    (with-run! [(rs/unsuspend-run! run-id)]
      (resume-run! (next-continuation! event-id) result))))

(defrecord StackFrame
  [address
   bindings
   result-key
   event-id
   expiry])

(defn next-continuation!
  "This function destructively updates the current run (popping the stack), so should
  only be called inside a dynamical context established by (with-run...).

  Returns a function (fn [result] ...) which returns result to the continuation at the top of the stack."
  ([] (next-continuation! nil))

  ([event-id]
   (let [[frame & rest-frames] (:stack *run*)
         address      (:address frame)
         result-key   (:result-key frame)
         bindings     (:bindings frame)
         continuation (fn [result]
                        (let [bindings-with-result (if result-key
                                                     (assoc bindings
                                                       result-key result)
                                                     bindings)]
                          (flow/continue address bindings-with-result)))]
     ;; for now, only allow responding to the event-id at the top of the stack
     (if frame
       (do
         (if-not (= event-id (:event-id frame))
           (throw (Exception. (format "Received event with mismatched event-id %s in run %s. Expecting %s"
                                event-id (:id *run*) (:event-id frame)))))

         (println "popping stack frame")
         (set! *run* (assoc *run* :stack rest-frames))
         continuation)
       ;; return nil if there is no next-continuation
       nil))))

(defn suspend
  "Used within deflow body to suspend execution until event with id=event-id is received."
  ([event-id] SUSPEND)
  ([event-id expiry] SUSPEND))

(defmacro resume-at
  "Generates code that continues execution at address after flow-form is complete.
  address - names the continuation
  params - list of parameters needed by the continuation
  result-key - the key to which the value of form will be bound in the continuation
  suspend - parameters provided to a `(suspend ...)` form or `true`
  body - expression which invokes a flow

  Returns:
  value of body
  "
  ([[address params result-key suspend] & body]
   (:pre [(instance? Address address)
          (vector? params)
          (or (nil? result-key) (symbol? result-key))
          (or (= true suspend) (vector? suspend))])
   (let [[event-id expiry] (if (vec suspend) suspend [])]
     `(let [bindings#  (hash-map ~@(flatten (map (fn [p] `('~p p)) params)))
            new-frame# (StackFrame. ~address bindings# '~result-key ~event-id ~expiry)]
        (println "inside resume-at, before stack push: \n\t*run*=" *run* "\n\tnew-frame=" new-frame#)
        (set! *run* (assoc *run* :stack (cons new-frame# (:stack *run*))))
        (println "inside resume-at, after stack push: *run*=" *run*)
        ~@body))))
