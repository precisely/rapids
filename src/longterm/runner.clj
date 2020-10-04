(ns longterm.runner
  (:require
    [longterm.runstore :as rs]
    [longterm.flow :as flow]
    [longterm.address :as address])
  (:import (longterm.address Address)))

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
  Run's `result` field."
  ([[run run-id] & body]
   `(binding [*run* (rs/unsuspend-run! ~run-id)]
      (let [~run *run*]
        (rs/save-run! (assoc ~run :state :running))
        (let [result# (do ~@body)
              state#  (if (suspend-signal? result#) :suspended :complete)]
          (rs/save-run! (assoc ~run :state state# :result result#)))))))

(defmacro start-run! [flow-form]
  (let [[op & args] flow-form
        address (address/create op)
        run     (rs/create-run!)]
    `(with-run! [~run (:id run)]
       (resume-run! ~run (fn [_] (flow/start ~address ~@args))))))

(defn resume-run!
  "Evaluates a continuation, popping the stack and passing the result to the next
   continuation until either a continuation returns SUSPEND or the stack is empty.

   This function destructively modifies"
  ([run continuation] (resume-run! run continuation nil))

  ([run continuation result]
   (let [next-result (continuation result)]
     (if (suspend-signal? next-result)
       next-result
       (let [next-continuation (next-continuation!)]
         (if next-continuation          ;
           (recur run next-continuation next-result)
           next-result))))))            ; final result returned

(defn process-event!
  "Processes an external event, causing the appropriate continuation to fire.
   Event is a map containing {:id event-id :result value}"
  [event]
  (let [run-id   (:run-id event)
        event-id (:event-id event)
        result   (:result event)]
    (with-run! [run (rs/get-run run-id)]
      (resume-run! run (next-continuation! event-id) result))))

(defrecord StackFrame
  [address
   bindings
   result-key
   event-id
   expiry])                             ; a symbol or vector of symbols

;; TODO: consider converting this to use trampoline
(defn next-continuation!
  "This function destructively updates the current run (popping the stack), so should
  only be called inside a dynamical context established by (with-run...).

  Returns a function (fn [result] ...) which returns result to the continuation at the top of the stack."
  ([] (next-continuation! nil))

  ([event-id]
   (let [[frame & rest-frames] (:stack *run*)
         continuation (fn [result]
                        (let [address              (:address frame)
                              result-key           (:result-key frame)
                              bindings             (:bindings frame)
                              bindings-with-result (if result-key
                                                     (assoc bindings
                                                       result-key result)
                                                     bindings)]
                          (flow/continue address bindings-with-result)))]
     ;; for now, only allow responding to the event-id at the top of the stack
     (if-not (= event-id (:eventid frame))
       (Exception. (format "Mismatched event-id %s in top frame. Expecting %s"
                     event-id (:event-id frame))))

     (set! *run* (assoc *run* :stack rest-frames))
     continuation)))

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
  body - expression which invokes a flow"
  ;; this form is used for suspend
  ([[address params result-key suspend] & body]
   (:pre (instance? Address address))
   (assert (vector? params))
   (let [[event-id expiry] (if (vec suspend) suspend [])]
     `(let [bindings#  (hash-map ~@(flatten (map (fn [p] `('~p p)) params)))
            new-frame# (StackFrame. ~address bindings# '~result-key ~event-id ~expiry)]
        (set! *run* (assoc *run* :stack (cons new-frame# (:stack *run*))))
        ~@body))))
