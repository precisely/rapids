(ns longterm.partition-set
  (:use longterm.stack))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add create combine)

;;; ContinuationSet

(defrecord Partition [params body])

(defn create []
  {})

(defn add
  [pset address params body]
  (assoc pset address (Partition. params body)))

(defn delete
  [pset address]
  (dissoc pset address))

(defn continuation-definition
  "Returns the s-expr representing the continuation at address"
  [pset address]
  (let [cdef (get pset address)]
    `(fn [& {:keys ~(:params cdef)}] ~@(:body cdef))))

(defn combine
  [pset & psets]
  (if (> (count psets) 0)
    (let [pset1    pset
          [pset2 & rest-psets] psets
          new-pset (if pset1
                     (if pset2
                       (merge pset1 pset2)
                       pset1)
                     pset2)]
      (recur new-pset rest-psets))
    pset))
