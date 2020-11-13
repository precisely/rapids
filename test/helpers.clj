(ns helpers)

(defmacro with-temp-ns [& body]
  `(let [cur-ns# (symbol (str (.getName *ns*)))]
     (in-ns '~(gensym))
     (try
       ~@body
       (finally (in-ns cur-ns#)))))