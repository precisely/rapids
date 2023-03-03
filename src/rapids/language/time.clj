;;
;; Simple time wrappers to allow joda-like usage:
;; (-> 3 days from-now)
;;
(ns rapids.language.time
  (:require [java-time.api :as t]
            [potemkin :refer [import-vars]]))

(defn now [] (t/local-date-time))

(defn from-now [offset] (t/plus (now) offset))
(defn ago [offset] (t/minus (now) offset))

(import-vars
  [java-time.api years months weeks days weeks hours minutes seconds])