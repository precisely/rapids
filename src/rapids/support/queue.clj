(ns rapids.support.queue
  (:import (clojure.lang PersistentQueue)
           (java.io Writer)))

(defn queue [& coll]
  (reduce conj PersistentQueue/EMPTY coll))

(defn install-queue-printer []
  (defmethod print-method PersistentQueue
    [q ^Writer w]
    (.write w "#queue ")
    (print-method (vec q) w)))
