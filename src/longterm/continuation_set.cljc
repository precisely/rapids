(ns longterm.continuation_set
  (:use longterm.stack))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add create combine)

;;; ContinuationSet

(defn create []
  {})

(defn add
  [cset address params body]
   (assoc-in cset [:bodies address]
     `(fn [& {:keys ~params}] ~@body)))

(defn cdef
  "Gets the continuation definition at address"
  [cset address]
  (get-in cset [:bodies address]))

(defn combine
  [cset1 cset2]
  (if cset1
    (if cset2
      (merge cset1 cset2))
      cset1)
    cset2)
