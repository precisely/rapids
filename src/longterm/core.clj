(ns longterm.core)

(defmacro defp
  "Define a long term procedure"
  [name args & code]
  `(defn ~name ~args ~@code))

(defmacro use-longterm
  "Imports all the longterm macros and functions"
  []
  (if &env
    (do
      (print "^&env is" &env)
      (let [current-namespace (symbol ((&env :ns) :name))]
        (when-not (= current-namespace 'longterm.core)
          `(~'ns ~current-namespace
             (:require [~'longterm.core :refer-macros [~'defp]])))))
    (print "&env is nil")))
