(ns longterm.jsonizer)

(defmulti to-json)

(defmethod