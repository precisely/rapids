(ns longterm.runloop
  (:require
    [longterm.runstore :as rs]
    [longterm.flow :as flow]
    [longterm.util :refer :all])
  (:import (longterm.address Address)))

(declare start! continue! resume-run!)
(declare resume-at next-continuation!)
(declare listen-signal?)

(declare process-run-result! with-run!)

;; helpers
(declare bindings-expr-from-params)

(defrecord StackFrame [address bindings result-key])

(def ^:dynamic *run*)

(defmacro with-run!
  "Takes a run-id, and obtains a run, transitions it to running state, clears the response list,
   and executes forms in body, then saves the run in :listening or :complete state.

  The final form of body returns a Listen record to put the run in :listening state. If it returns
  a value, the run state will change to :complete, and the value will be stored in the
  Run's `result` field.

  Returns:
    run - Run instance in :listening or :complete state"
  ([[run-form] & body]
   `(binding [*run* (assoc ~run-form :response [])]
      (let [value# (do ~@body)]
        (process-run-result! value#)))))

(defn respond!
  "Adds an element to the current run response: returns nil"
  [& responses]
  (set! *run* (assoc *run* :response (concat (:response *run*) (vec responses))))
  nil)

(defn start!
  "Starts a run with the flow and given arguments.
  Returns the Run in :listening or :complete state."
  [flow & args]
  {:pre  [(refers-to? flow/flow? flow)]
   :post [(rs/run-in-state? % :listening :complete)]}
  (with-run! [(rs/create-run! :running)]
    (resume-run! (fn [_]
                   (apply flow/entry-point flow args)))))

(alter-var-root #'flow/start-with-run-context! (constantly start!))

(defn resume-run!
  "Evaluates a par-bound continuation, popping the stack and passing the result to the next
   continuation until either a continuation returns a Listen instance or the stack is empty.

   This function destructively modifies the run, but DOES NOT save it. It should
   be wrapped inside a with-run! block."
  ([continuation] (resume-run! continuation nil))

  ([continuation result]
   {:pre [(rs/run-in-state? *run* :running)
          (or (fn? continuation) (nil? continuation))]}
   (if continuation
     (let [next-result (continuation result)]
       (if (listen-signal? next-result)
         next-result                                        ; return the listen signal, which ends the runlet
         (recur (next-continuation!) next-result)))
     result)))                                              ; return the final result (to be stored in run.result)

(defn check-permit [listen-permit event-permit]
  (if listen-permit
    (if (fn? listen-permit)
      (throw (Exception. "Function permits not yet supported"))
      (if-not (= listen-permit event-permit)
        (throw (Exception.
                 (format "Run is listening for event with permit %s but received %s"
                         listen-permit event-permit)))))))

(defn continue!
  "Processes an external event, finds the associated run and calls the continuation at the
  top of the stack, passing a result, which gets injected into the flow as the
  value of the (listen!...) call.

  Args:
    run-id - id the run to continue
    permit - if a listen! :permit value was provided, it must match
    result - optional result that will be provided as the result of the listen! expression

  Returns:
    run - in :listening or :complete state
  "
  ([run-id] (continue! run-id nil nil))

  ([run-id permit] (continue! run-id permit nil))

  ([run-id permit result]
   {:pre  [(not (nil? run-id))]
    :post [(rs/run-in-state? % :listening :complete)]}
   (with-run! [(rs/unlisten-run! run-id)]
     (let [stack (:stack *run*)
           top (first stack)]
       (if-not (listen-signal? top)
         (throw (Exception. (format "Invalid stack for run %s; expecting Listen frame but found %s" top))))
       (check-permit (:permit top) permit)
       ;; pop the Listen object at the top of the stack and continue executing
       (set! *run* (assoc *run* :stack (rest stack)))
       (resume-run! (next-continuation!) result)))))

;;
;; Helpers
;;
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
        (flow/exec address bindings-with-result)))))

(defn next-continuation!
  "This function destructively updates the current run (popping the stack), so should
  only be called inside a dynamical context established by (with-run...).

  Returns:
   function (fn [result] ...) which returns result to the continuation at the top of the stack
   or nil if stack is empty"
  []
  (let [[top & rest-frames] (:stack *run*)]
    (when top
      (set! *run* (assoc *run* :stack rest-frames))
      (continuation-from-frame top))))

(defmacro resume-at
  "Generates code that continues execution at address after flow-form is complete.
  address - names the continuation
  params - list of parameters needed by the continuation
  result-key - the key to which the value of form will be bound in the continuation
  body - expression which invokes a flow

  Returns:
  value of body"
  ([[address params result-key] & body]
   (:pre [(instance? Address address)
          (vector? params)
          (or (nil? result-key) (symbol? result-key))])
   (let [bindings-expr (bindings-expr-from-params params)]
     `(let [bindings# ~bindings-expr
            new-frame# (StackFrame. ~address bindings# '~result-key)]
        (set! *run* (assoc *run* :stack (cons new-frame# (:stack *run*))))
        ~@body))))

;;
;; Listen
;;

(defrecord Listen [permit expires result])

(defn listen-signal? [x] (instance? Listen x))

(defn listen!
  "Used within deflow body to listen execution until event with id=context is received.
  Args:
    permit - a keyword or a permit function
       a permit function tests the permit value provided by a continue! call
       it has the form  (fn [permit-val {:keys [...params...]})
       where params are variables from the current lexical context needed
       to verify the continue! permit value.
       NOTE: permit functions are not yet implemented
    expires - when listening should expire
              a Java LocalDateTime or a vector [JavaLocalDateTime, result]
              where result = the result returned by the (listen!..) expr
              on expiry
  Examples:
  (deflow foo [z]
     (let [no-permit-required (listen!)
           only-allow-button-press (listen! :permit :button-press)
           expire-in-10-min (listen! :expires (-> 10 minutes from-now))
           expire-with-value (listen! :expires [(-> 10 minutes from-now), :expiry-result])]
       ... do something))"
  [& {:keys [permit expires]}]
  (if-not (and (bound? #'*run*)
               (rs/run-in-state? *run* :any))
    (throw (Exception. (format "Invalid run context while evaluating (listen!%s%s)"
                               (if permit (str " :permit " permit))
                               (if expires (str " :expiry " expires)))))
    (let [[expire-time, result] (if (vector? expires) expires [expires, nil])]
      (Listen. permit expire-time result))))

;;
;; Helpers
;;

(defn- run-is-valid?
  [run]
  (let [stack (:stack run)
        state (:state run)
        top (first stack)
        result (and (every? #(or (instance? StackFrame %) (listen-signal? %)) stack)
                    (case state
                      :listening (listen-signal? top)
                      :running (instance? StackFrame top)
                      :complete (empty? stack)))]
    result))

(defn- process-run-result! [value]
  {:post [(run-is-valid? *run*) "Failure in `process-run-result!`"]}
  (let [current-stack (:stack *run*)
        [state, stack, result] (if (listen-signal? value)
                                 [:listening, (cons value, current-stack), nil]
                                 [:complete, current-stack, value])]
    (set! *run* (assoc *run* :state state :result result :stack stack))
    (rs/save-run! *run*)))

(defn bindings-expr-from-params [params]
  `(hash-map ~@(apply concat (map #(vec `('~% ~%)) params))))