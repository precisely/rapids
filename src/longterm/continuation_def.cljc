(ns longterm.continuation_def
  (:use longterm.stack)
  (:require [longterm.stack :as thunk]))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add continue create combine)

;;; ContinuationSet

(defrecord ContinuationDef
  [body params dest-address result-key])

(defn resolved? [cdef]
  (not (nil? (:next-address cdef))))

(defn resolved-body [cdef]
  (let [next-address (:next-address cdef)
        result-key (:result-key cdef)
        bindings (:params cdef)
        resume-at-args (if (and next-address result-key)
                         `[,next-address ,result-key]
                         `[,next-address])

  (if n
    `(resume-at [,

(defn fndef [cdef]
  `(fn [& {:keys ~(:params cdef)}] ~@(resolved-body cdef)))

