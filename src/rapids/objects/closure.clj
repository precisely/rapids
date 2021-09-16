;;;
;;; Enables persisting a closure across partitions. The strategy is to store the
;;; function as a continuation in the Flow :continuations,
;;;
(ns rapids.objects.closure
  (:require [rapids.objects.flow :as flow]
            [rapids.support.defrecordfn :refer [defrecordfn]]
            [rapids.support.util :refer [in? unqualified-symbol?]]))


(defrecordfn Closure [address bindings]
  (fn [this & args]
    (let [closure-fn (flow/exec (:address this) (:bindings this))]
      (apply closure-fn args))))

(defn closure? [o] (instance? Closure o))

