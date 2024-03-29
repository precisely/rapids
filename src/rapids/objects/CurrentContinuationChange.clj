;;
;; CurrentContinuationChange is thrown by callcc and caught by the eval-loop.
;;   The eval-loop uses the information to continue the computation with the
;;   desired stack, bindings and value.
;;
(ns rapids.objects.CurrentContinuationChange
   (:gen-class
    :extends java.lang.Throwable
    :constructors {[clojure.lang.ISeq clojure.lang.PersistentVector Object] []}                ; mapping of my-constructor -> superclass constuctor
    :init init
    :state state
    :methods [[stack [] clojure.lang.ISeq]                  ; the new stack
              [dynamics [] clojure.lang.PersistentVector]   ; the new dynamic bindings
              [input [] java.lang.Object]]                   ; input to be provided to the next stack-fn
    :main false))

(defn -init
  [stack dynamics input]
  [[] [stack, dynamics, input]])

(defn -stack [this]
  (-> this .state first))

(defn -dynamics [this]
  (-> this .state second))

(defn -input [this]
  (-> this .state (nth 2)))
