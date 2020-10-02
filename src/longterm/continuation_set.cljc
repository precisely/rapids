(ns longterm.continuation-set
  (:use longterm.stack))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add create combine)

;;; ContinuationSet

(defrecord ContinuationDef [params body])

(defn create []
  {})

(defn add
  [cset address params body]
  (assoc cset address (ContinuationDef. params body)))

(defn delete
  [cset address]
  (dissoc cset address))

(defn continuation-definition
  "Returns the s-expr representing the continuation at address"
  [cset address]
  (let [cdef (get cset address)]
    `(fn [& {:keys ~(:params cdef)}] ~@(:body cdef))))

(defn combine
  [cset & csets]
  (if (> (count csets) 0)
    (let [cset1    cset
          [cset2 & rest-csets] csets
          new-cset (if cset1
                     (if cset2
                       (merge cset1 cset2)
                       cset1)
                     cset2)]
      (recur new-cset rest-csets))
    cset))
