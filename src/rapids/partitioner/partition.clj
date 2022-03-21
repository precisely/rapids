(ns rapids.partitioner.partition
  (:import (clojure.lang IPersistentVector)))


;;; A Partition is a signature which will be used to create a partition function
;;;
(defrecord Partition
  [^IPersistentVector params
   ^IPersistentVector body
   ^Boolean suspending
   ^Boolean final])           ; if true, partition body resumes at another address, so cannot be linked

(defn ->partition
  "Returns a partition. If final is true, it means the partition body can no longer be changed."
  [params body & {:keys [suspending final]}]
  {:pre [(sequential? params) (vector? body)]}
  (->Partition (vec params) body (boolean suspending) (boolean final)))

(defn partition? [o] (instance? Partition o))

(defn resuming-at
  "Updates the partition so that the suspending, non-final expression resumes at the given address providing its value
  to input-key."
  [p addr input-key]
  {:pre [(:suspending p) (not (:final p))]}
  (-> p
    (update :body (fn [expr] `(rapids.partitioner.resume-at/resume-at [~addr ~(:params p) ~input-key]
                                ~@(:body p))))
    (assoc :final true)))
