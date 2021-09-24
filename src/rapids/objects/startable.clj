(ns rapids.objects.startable
  (:import (clojure.lang Symbol Var)))

(defprotocol Startable
  (call-entry-point [this args]))

(extend Symbol
  Startable
  {:call-entry-point (fn [this args]
                       (call-entry-point (resolve this) args))})

(extend Var
  Startable
  {:call-entry-point (fn [this args]
                       (call-entry-point (var-get this) args))})

(defn startable? [o]
  (satisfies? Startable o))
