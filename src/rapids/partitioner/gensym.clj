(ns rapids.partitioner.gensym
  (:require [org.apache.commons.lang3.reflect.FieldUtils :as field-utils])
  (:import (clojure.lang RT)
           (java.util.concurrent.atomic AtomicInteger)))

(def original-gensym gensym)
(def ^:dynamic *gensym-counter*)
(defn stable-gensym
  ([] (stable-gensym "G__"))
  ([s] (symbol (str s (swap! *gensym-counter* inc)))))

(defmacro with-stable-gensym [& body]
  `(let [prev-id#            (field-utils/*read-static-field RT "id" true)]
     (binding [*gensym-counter* (atom 0)]
       (alter-var-root #'gensym (constantly stable-gensym))
       (try
         (let [result# (do ~@body)]
           (field-utils/*write-static-field RT "id" (AtomicInteger. 0) true)
           result#)
         (finally
           (field-utils/*write-static-field RT "id" prev-id# true)
           (alter-var-root #'gensym (constantly original-gensym)))))))

