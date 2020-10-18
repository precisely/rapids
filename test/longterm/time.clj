(ns longterm.time
  (:require [java-time :as t]
            [potemkin :refer [import-vars]]))

(defn from-now [offset] (t/plus (t/local-date) offset))

(import-vars
  [java-time days weeks months years seconds hours weeks])