(ns longterm.continuation)

;;;; Continuation

;;; Thunks Continuations replace longterm flow expressions in function bodies
(defrecord ContinuationId [flow-name hash n])
(defrecord ContinuationDef [params body])
(defn next-continuation-id [cid] (assoc cid :n (inc (:n cid))))
(defn make-continuation-form
  [cdef]
  `(fn [~@(:params cdef)] ~@(:body cdef)))

(defn continuation-from-id [continuation-id]
  (let [[flow-name hash n] continuation-id]
    (-> (resolve flow-name) :continuations (nth n))))