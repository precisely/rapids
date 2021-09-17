;;;
;;; Enables persisting a closure across partitions. The strategy is to generate a flow
;;; continuation which returns the closure. I.e., the continuation (stored in Flow :continuations)
;;; looks like:
;;; #Flow{:name myflow
;;;       :continuations {closure-address (fn [bindings]
;;;                                          (fn ([argument-list...] ...) ([....] ...)))
;;;                       ...}
;;;       ...}
;;; The closure record described here allows for persisting the closure in the stack. It
;;; can be saved to persistent storage (unlike an actual Clojure closure) because it only
;;; contains the address of the clojure function and the bindings.
;;;
(ns rapids.objects.closure
  (:require [rapids.objects.flow :as flow]
            [rapids.objects.address :as a]
            [rapids.support.defrecordfn :refer [defrecordfn]]
            [rapids.support.util :refer [in? unqualified-symbol?]]))

(defrecordfn Closure [address bindings]
  (fn [this & args]
    (let [closure-fn (flow/exec (:address this) (:bindings this))] ; generate the closure with the bindings
      (apply closure-fn args))))

(defn closure? [o] (instance? Closure o))

(defn closure-name [c]
  (-> c :address a/to-string))