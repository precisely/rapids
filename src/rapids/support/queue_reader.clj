;;
;; Convenience methods for dealing with queues
;;
(ns rapids.support.queue-reader
  (:import (clojure.lang PersistentQueue)))

(defn queue-reader
  ([] PersistentQueue/EMPTY)
  ([coll]
   (let [coll (if (list? coll) `(list ~@coll) coll)]
     `(reduce conj PersistentQueue/EMPTY ~coll))))
