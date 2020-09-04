(ns longterm.core)

(defmacro defp
  "Define a long term procedure"
  [name args & code]
  `(defn ~name ~args ~@code))

;; Use this macro at the top of a longterm code:
;; (ns foo.bar (:require [longterm.core :as lt]))
;; (lt/uselt)
(defmacro uselt
  "Imports all the longterm macros and functions"
  ([ns-symbol]
   `(~'ns ~ns-symbol (:require [longterm.core :refer-macros [~'defp]])))

  ([]
   (println ":name &env" (if &env (:name &env) "&env unbound"))
   (println "ns-name *ns*" (if *ns* (ns-name *ns*) "*ns* unbound"))
   (cond
     *ns* `(uselt ~(symbol (ns-name *ns*)))
     &env `(uselt ~(symbol (:name &env)))
     :else (throw "Unable to determine current namespace"))))