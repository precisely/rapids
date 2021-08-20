(ns rapids.storage.persistence
  (:require [taoensso.nippy :as n]))

(defn freeze [o]
  (n/freeze o))

(defn thaw [o]
  (n/thaw o))

(defmacro extend-freeze [type custom-type-id [x out] & body]
  `(n/extend-freeze ~type ~custom-type-id [~x ~out] ~@body))

(defmacro extend-thaw [custom-type-id [in] & body]
  `(n/extend-thaw ~custom-type-id [~in] ~@body))
