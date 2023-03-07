(ns hooks.rapids
  (:require [clj-kondo.hooks-api :as api]))

;; (defn with-bound [{:keys [:node]}]
;;   (let [[binding-vec & body] (rest (:children node))
;;         [sym val opts] (:children binding-vec)]
;;     (when-not (and sym val)
;;       (throw (ex-info "No sym and val provided" {})))
;;     (let [new-node (api/list-node
;;                     (list*
;;                      (api/token-node 'let)
;;                      (api/vector-node [sym val])
;;                      opts
;;                      body))]
;;       {:node new-node})))

(defn with-bound [{:keys [:node]}]
  (let [[binding-vec & body] (rest (:children node))
        [sym val opts] (:children binding-vec)]
    (when-not (and sym val)
      (throw (ex-info "No sym and val provided" {})))
    (let [new-node (api/list-node
                    (list*
                     (api/token-node 'let)
                     (api/vector-node [sym val])
                     opts
                     body))]
      {:node new-node})))