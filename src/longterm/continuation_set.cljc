(ns longterm.continuation-set
  (:use longterm.stack))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add create combine)

;;; ContinuationSet

(defn create []
  {:bodies {}})

(defn add
  [cset address params body]
  (assoc-in cset [:bodies address]
    `(fn [& {:keys ~params}] ~@body)))

(defn cdef
  "Gets the continuation definition at address"
  [cset address]
  (get-in cset [:bodies address]))

(defn combine
  [cset & csets]
  (if (> (count csets) 0)
    (let [cset1    cset
          [cset2 & rest-csets] csets
          new-cset (if cset1
                     (if cset2
                       (assoc cset1
                         :bodies (merge (:bodies cset1) (:bodies cset2)))
                       cset1)
                     cset2)]
      (recur new-cset rest-csets))
    cset))
