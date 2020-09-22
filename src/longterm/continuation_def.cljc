(ns longterm.continuation_def
  (:use longterm.thunk)
  (:require [longterm.thunk :as thunk]))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add continue create combine)

;;; ContinuationSet

;; Partitioning functions sometimes need to leave undefined the precise address
;; of a partition connects to until a later point in the computation.
;; For example, when processing an series of arguments, it isn't clear
;; until the end of the argument list is reached whether the next address
;; should be the next partition in the arglist or the partition containing
;; the function call.
;; The ContinuationSet leaving function definitions unresolved as follows:
(defrecord ContinuationSet
  ; bodies: map of address => resolved or unresolved body
  ;            1) RESOLVED body - a list of expressions representing
  ;               the code of a partition. A body is considered "resolved"
  ;               because the destination address (if any) has already been
  ;               interpolated into the code
  ;            2) UNRESOLVED body: a function of the form (fn [dest-address] ...) which
  ;            interpolates dset-address into a list of expressions and returns it
  ; params - map of address => parameter vectors
  ; connections - set of [src dest] address pairs, used for debugging
  [bodies
   params
   result-keys
   connections])

(defrecord ContinuationDef
  [body params dest result-key])

(defn resolved? [cdef]
  (not (nil? (:dest cdef))))

(defn resolved-body [cdef]
  ((:body cdef) (:dest cdef) (:result-key cdef)))

(defn resolve
  [cdef address]
  (assoc-in cdef
(defn fndef [cdef]
  `(fn [& {:keys ~(:params cdef)}] ~@(resolved-body cdef)))

