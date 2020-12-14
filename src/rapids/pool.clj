(ns rapids.pool
  (:require [rapids.util :refer [new-uuid]]))

(defrecord Pool [id order])

(def PoolOrder #{:fifo :filo :ranked})
(defn pool [& {:keys [order] :or {order :filo}}]
  (Pool. (new-uuid) order))

(defn take-out!
  ([pool] (take-out pool nil))
  ([pool default]
   ()))

(defn put-in! [pool data]
  )