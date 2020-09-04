(ns longterm.core
  (:require-macros [longterm.core]))

(defn main []
  (enable-console-print!)
  (prn "Hey there!")
  (prn "Hello, World!"))

(main)
