(ns rapids.objects.startable
  (:require [rapids.support.util :refer [dereference]])
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

;(defn name [obj]
;  (get-name (dereference obj)))

;(defn call
;  "Calls Startable object with given args.
;
;  obj - symbol, var or Callable object
;  args - list of arguments"
;  [obj args]
;  (if (startable? obj)
;    (call-entry-point obj args)
;    (throw (ex-info "Attempt to call object which does not support Startable protocol."
;             {:type   :runtime-error
;              :object obj}))))
