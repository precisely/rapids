;;
;; The CacheProxy class
;;
(ns rapids.runtime.CurrentContinuationChange
   (:gen-class
    :extends java.lang.Throwable
    :constructors {[clojure.lang.ISeq clojure.lang.PersistentVector Object] []}                ; mapping of my-constructor -> superclass constuctor
    :init init
    :state state                                            ; name for the var that holds your internal state
    :methods [[stack [] clojure.lang.ISeq]
              [dynamics [] clojure.lang.PersistentVector]
              [data [] java.lang.Object]]
    :main false))

(defn -init
  [stack dynamics data]
  [[] [stack, dynamics, data]])

(defn -stack [this]
  (-> this .state first))

(defn -dynamics [this]
  (-> this .state second))

(defn -data [this]
  (-> this .state (nth 2)))