(ns longterm.synchronization
  (:require [longterm.deflow :refer [deflow]]
            [longterm.runloop :refer [with-run! *run*]]
            [longterm.util :refer [in?]]
            [longterm.runstore :as rs])
  (:import (longterm.runloop Suspend)))

;; this fn is treated as a special form by the partitioner
;; the argument is evaluated as is
(defn >> [run]
  "Redirection operator: transfers execution to `run` until it changes to blocking state.

  Returns the `run`
  If "
  (let [current-run *run*
        current-response (:response *run*)
        new-response (:response run)]
    (set! *run* (assoc *run*
                  :stack (cons (Suspend. 1 2 3) nil)
                  :state :redirected :response [] :redirect (:id run)))
    (rs/save-run! *run*)
    (set! *run* (assoc run :response (concat current-response new-response)
                           :return (:id current-run)))))

(defn <!
  "The blocking operator prevents the current run from continuing until the provided run is finished.
  Returns the result of the provided run.

  Example:
  (deflow blocked-flow []
     (let [result (<! (start! myflow 1 2 3))] ; start ensures a new run is created
        (respond! \"Got the result:\" result)))"
  [run]
  (let [blocker (assoc run :blocking *run*)] ; provided run blocks the current run
    (set! *run* (assoc *run*, :state :blocked, :blocker blocker))))

(defn unblock!
  "Used internally when a blocking run completes to unblock the blocked run."
  [run]
  {:pre [(rs/run-in-state? run :blocked)]}
  ())



