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
            [rapids.objects.startable :as s]
            [rapids.objects.address :as a]
            [rapids.support.defrecordfn :refer [defrecordfn]]
            [rapids.support.util :refer [in? unqualified-symbol?]])
  (:import (rapids.objects.startable Startable)
           (clojure.lang Named)))

(defrecordfn Closure
  [address bindings suspending?]
  :fn (fn [this & args]
        (if suspending?
          (throw (ex-info "Attempt to call suspending closure directly. Use fcall, fapply or start!"
                   {:object this}))
          (s/call-entry-point this args)))

  Named
  (getNamespace [this] (.getNamespace (-> this address symbol)))
  (getName [this] (-> this address symbol a/to-string))

  Startable
  (s/call-entry-point [this args]
    (let [closure-fn (flow/call-continuation (:address this) (:bindings this))] ; generate the closure with the bindings
      (apply closure-fn args))))

(defn closure? [o] (instance? Closure o))

(defn closure-name [c]
  (-> c :address a/to-string))