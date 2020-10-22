(ns longterm.runloop
  (:require
    [longterm.runstore :as rs]
    [longterm.flow :as flow]
    [longterm.util :refer :all])
  (:import (longterm.address Address)
           (java.time LocalDateTime)))

(declare start! continue! resume-run!)
(declare resume-at next-continuation!)
(declare suspend-signal?)

(declare process-run-result! with-run!)

;; helpers
(declare bindings-expr-from-params redirect-to)

(defrecord StackFrame [address bindings result-key])
(defn stack-frame? [o] (instance? StackFrame o))

(def ^:dynamic *run*)

(defmacro with-run!
  "Takes a run-id, and obtains a run, transitions it to running state, clears the response list,
   and executes forms in body, then saves the run in :suspended or :complete state.

  The final form of body returns a Suspend record to put the run in :suspended state. If it returns
  a value, the run state will change to :complete, and the value will be stored in the
  Run's `result` field.

  Returns:
    run - a Run instance in :suspended or :complete state. Note that redirection
          during execution of body can result in a different run being returned
          than the one retuned by run-form."
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
  Returns the Run in :suspended or :complete state."
  [flow & args]
  {:pre  [(refers-to? flow/flow? flow)]
   :post [(rs/run-in-state? % :suspended :complete)]}
  (with-run! [(rs/create-run! :running)]
    (resume-run! (fn [_]
                   (apply flow/entry-point flow args)))))

;; Solves a circular dependency problem - see longterm.flow for details
(alter-var-root #'flow/start-with-run-context! (constantly start!))

(defn resume-run!
  "Evaluates a par-bound continuation, popping the stack and passing the result to the next
   continuation until either a continuation returns a Suspend instance or the stack is empty.

   This function destructively modifies the run, but DOES NOT save it. It should
   be wrapped inside a with-run! block."
  ([continuation] (resume-run! continuation nil))

  ([continuation result]
   {:pre [(rs/run-in-state? *run* :running)
          (or (fn? continuation) (nil? continuation))]}
   (if continuation
     (let [next-result (continuation result)]
       (if (suspend-signal? next-result)
         next-result          ; return the suspend signal, which ends the runlet
         (recur (next-continuation!) next-result)))
     result)))                ; return the final result (to be stored in run.result)

(defn check-permit [suspend-permit event-permit]
  (if suspend-permit
    (if (fn? suspend-permit)
      (throw (Exception. "Function permits not yet supported"))
      (if-not (= suspend-permit event-permit)
        (throw (Exception.
                 (format "Attempt to continue! Run with permit %s but received %s"
                   suspend-permit event-permit)))))))

(defn continue!
  "Processes an external event, finds the associated run and calls the continuation at the
  top of the stack, passing a result, which gets injected into the flow as the
  value of the (suspend!...) call.

  Args:
    run-id - id the run to continue
    permit - if a suspend! :permit value was provided, it must match
    result - optional result that will be provided as the result of the suspend! expression
    responses - initial responses (used when resuming the run after redirection)

  Returns:
    run - in :suspended or :complete state
  "
  ([run-id] (continue! run-id nil nil []))

  ([run-id permit] (continue! run-id permit nil []))

  ([run-id permit result] (continue! run-id permit result []))

  ([run-id permit result responses]
   {:pre  [(not (nil? run-id))]
    :post [(rs/run-in-state? % :suspended :complete)]}
   (with-run! [(rs/acquire-run! run-id)]
     (let [suspend       (:suspend *run*)
           id            (:id *run*)]
       (if-not (suspend-signal? suspend)
         (throw (Exception. (format "Attempt to continue Run %s with invalid :suspend value: %s" id suspend))))
       (check-permit (:permit suspend) permit)
       ;; clear the Suspend object and set initial responses
       (set! *run* (assoc *run* :suspend nil :responses responses))
       (resume-run! (next-continuation!) result)))))

;;
;; Helpers
;;
(defn- continuation-from-frame [frame]
  {:pre [(instance? StackFrame frame)]}
  (let [address    (:address frame)
        result-key (:result-key frame)
        bindings   (:bindings frame)]
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
     `(let [bindings#  ~bindings-expr
            new-frame# (StackFrame. ~address bindings# '~result-key)]
        (set! *run* (assoc *run* :stack (cons new-frame# (:stack *run*))))
        ~@body))))

;;
;; Suspend
;;
(def ^:constant SuspendMode [:block :redirect nil])
(defrecord Suspend
  [permit                     ; the permit must match the value provided to continue!
   expires                    ; java.time.LocalDateTime
   result                     ; if Suspend expires, this value is used
   mode])                     ; a SuspendMode enum value

(defn suspend-signal? [x] (instance? Suspend x))

(defn valid-suspend?
  [s]
  (and
    (instance? Suspend s)
    (or (nil? (:expires s))
      (instance? LocalDateTime (:expires s)))
    (in? SuspendMode (:mode s))))

(defn suspend!
  "Used within deflow body to suspend execution until event with id=context is received.
  Args:
    permit - a keyword or a permit function
       a permit function tests the permit value provided by a continue! call
       it has the form  (fn [permit-val {:keys [...params...]})
       where params are variables from the current lexical context needed
       to verify the continue! permit value.
       NOTE: permit functions are not yet implemented
    expires - when suspending should expire
              a Java LocalDateTime or a vector [JavaLocalDateTime, result]
              where result = the result returned by the (suspend!..) expr
              on expiry
  Examples:
  (deflow foo [z]
     (let [no-permit-required (suspend!)
           only-allow-button-press (suspend! :permit :button-press)
           expire-in-10-min (suspend! :expires (-> 10 minutes from-now))
           expire-with-value (suspend! :expires [(-> 10 minutes from-now), :expiry-result])]
       ... do something))"
  [& {:keys [permit expires mode]}]
  {:post [(valid-suspend? %)]}
  (if-not (and (bound? #'*run*)
            (rs/run-in-state? *run* :any))
    (throw (Exception. (format "Invalid run context while evaluating (suspend!%s%s)"
                         (if permit (str " :permit " permit))
                         (if expires (str " :expiry " expires)))))
    (let [[expire-time, result] (if (vector? expires) expires [expires, nil])
          suspend (Suspend. permit expire-time result mode)]
      (set! *run* (assoc *run*
                    :state :suspended,
                    :suspend suspend))
      (rs/save-run! *run*)
      suspend)))

(defmacro <!
  "Blocking operator - suspends the current run and starts a run in :blocker mode"
  [run-expr & {:keys [expires]}]
  `(let [run# (assoc ~run-expr :mode :blocker :parent-run-id (:id *run*))]
     (if (rs/run-in-state? run# :running :suspended)
       (suspend! :permit (:id run#) :expires expires)
       (if (rs/run-in-state? run# :complete)
         (:result run#)))))


(defn move-response [from-run to-run]
  "Returns both runs with updated responses; the response of from-run will start the response of to-run"
  (let [from-response (:response from-run)
        to-response   (:response to-run)]
    [(assoc from-run :response []),
     (assoc to-run :response (concat from-response to-response))]))

(defn >>
  "Redirect operator - suspends the current run and starts a run in :redirected mode"
  [run & {:keys [expires]}]
  `(let [run# (assoc ~run,
                :mode :redirected,
                :parent-run-id (:id *run*))]
     (cond
       (rs/run-in-state? run# :suspended)
       (let [[current-run, run#] (move-response *run*, run#)]
         (set! *run* run#)    ; redirects to the new run
         (binding [*run* current-run]
           (suspend! :permit (:id run) :expires expires)))

       (rs/run-in-state? run :complete))))

;;
;; Helpers
;;

(defn- run-is-valid?
  [run]
  (let [{stack :stack
         state :state} run
        top    (first stack)
        result (and (every? stack-frame? stack)
                 (case state
                   :suspended (suspend-signal? (:suspend run))
                   :running (instance? StackFrame top)
                   :complete (empty? stack)))]
    result))

(defn- redirect-to!
  "Returns the result to run, copying response to it also. Returns the new run."
  [run-id result response]
  {:pre [(not (nil? run-id))]}
  (set! *run* (continue! run-id (:id *run*) result response))
  (rs/save-run! *run*)
  *run*)

(defn- save-run-result!
  [result]
  (set! *run* (assoc *run* :state :complete :result result :parent-run-id nil :mode nil))
  (rs/save-run! *run*)
  *run*)

(defn- process-run-result!
  [value]
  (let [{parent-run-id :parent-run-id,
         response      :response,
         run-mode      :mode} *run*
        [{suspend-mode :mode}, suspend] (if (suspend-signal? value) [value, value] [{}, nil])]
    (if suspend
      ;; if we received a suspend signal...

      ;; redirected runs return to parent when they receive a blocking suspend
      (if (= [run-mode, suspend-mode] [:redirected, :block])

        ;; return the parent run
        (redirect-to! parent-run-id *run* response)

        ;; otherwise, just return the suspended run
        *run*)

      ;; the run has completed
      (do
        (save-run-result! value)

        ;; if current run is blocking another run...
        (if (= run-mode :blocker)

          ;; return the result to the parent run, redirecting execution there...
          (redirect-to! parent-run-id value [])

          ; otherwise, we're done - return the current, completed run
          *run*)))))

;(defn- process-run-result!
;  [value]
;  (let [result-run (cond
;
;                     ;; when a redirected run blocks or completes,
;                     (redirect-ended? *run* value)
;                     (do
;                       (rs/save-run! (assoc *run*, :state :suspended, :suspend value))
;                       (continue! (:parent-run-id *run*) (:id *run*) *run* (:response *run*)))
;
;                     (blocker-completed? *run* value)
;                     (do
;                       (rs/save-run! (assoc *run*, :state :complete, :result value, :suspend nil))
;                       (continue! (:parent-run-id *run*) (:id *run*) value))
;
;                     (suspend-signal? value)
;                     (assoc *run*, :state :suspended, :suspend value)
;
;                     :else
;                     (assoc *run*, :state :complete, :result value))]
;    (rs/save-run! result-run)
;    result-run))

;(defn- process-run-result! [value]
;  {:post [(run-is-valid? *run*) "Failure in `process-run-result!`"]}
;  (let [current-stack (:stack *run*)
;
;        [state, stack, result] (if (suspend-signal? value)
;                                 [:suspended, (cons value, current-stack), nil]
;                                 [:complete, current-stack, value])]
;    (if (end-redirect? *run* value)
;
;      (set! *run* (assoc *run* :state state :result result :stack stack))
;      (rs/save-run! *run*)))

(defn bindings-expr-from-params [params]
  `(hash-map ~@(apply concat (map #(vec `('~% ~%)) params))))