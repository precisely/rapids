(ns rapids.run
  (:require
    [rapids.util :refer [in? new-uuid ifit linked-list? key-to-str]]
    [rapids.storage.protocol :refer [to-storage-record from-storage-record]]
    [rapids.signals :as signals]
    [rapids.address :as a]
    [taoensso.nippy :refer [freeze thaw]]
    [clojure.spec.alpha :as s]
    [rapids.stack-frame :as sf]
    [java-time :as t]
    [clojure.tools.macro :refer [macrolet]])
  (:import (java.util UUID)
           (java.time LocalDateTime)))

(declare run-in-state? set-storage! create-run! save-run! get-run acquire-run!)

(defrecord Run [id state stack result run-response response])

(defn run? [run] (instance? Run run))

(def ^:const RunStates #{:created :suspended :running :error :complete})
(defn run-in-state?
  [run & states]
  (let [state (:state run)
        result (and (instance? Run run) (or (in? states state) (in? states :any)))]
    result))

(def ^:const ReturnModes #{:redirect :block})               ; semantics for returning to parent
(defn run-in-mode? [run & return-modes]
  (and (instance? Run run)
    (or (in? return-modes (:return-mode run)) (in? return-modes :any))))

(def FrozenClass (.getClass (byte-array 0)))
(defn frozen? [o] (instance? FrozenClass o))

(s/def ::run (s/keys
               :req-un [::id, ::state]
               :opt-un [::stack, ::suspend, ::result, ::response, ::run-response,
                        ::return-mode, ::parent-run-id, ::error,
                        ::next, ::next-id
                        ::updated-at, ::created-at]))

(s/def ::id uuid?)
(s/def ::error #(instance? Exception %))
(s/def ::next ::run)
(s/def ::next-id ::id)
(s/def ::parent-run-id ::id)
(s/def ::response vector?)
(s/def ::result (constantly true))
(s/def ::return-mode ReturnModes)
(s/def ::run-response vector?)
(s/def ::state RunStates)
(s/def ::stack (s/and seq? (s/* sf/stack-frame?)))
(s/def ::start-form string?)
(s/def ::suspend signals/suspend-signal?)

(s/def ::frozen frozen?)
(s/def ::datetime #(instance? LocalDateTime %))

(s/def :record/id ::id)
(s/def :record/error (s/nilable ::frozen))
(s/def :record/expires (s/nilable ::datetime))
(s/def :record/next_id (s/nilable ::id))
(s/def :record/parent_run_id (s/nilable ::id))
(s/def :record/response ::frozen)
(s/def :record/result (s/nilable ::frozen))
(s/def :record/return_mode (s/nilable (set (map #(name %) ReturnModes))))
(s/def :record/run_response ::frozen)
(s/def :record/state (set (map #(name %) RunStates)))
(s/def :record/stack ::frozen)
(s/def :record/start_form (s/nilable string?))
(s/def :record/suspend (s/nilable ::frozen))
(s/def :record/suspend_expires (s/nilable ::datetime))
(s/def :record/suspend_permit (s/nilable string?))
(s/def :record/updated_at ::datetime)
(s/def :record/created_at ::datetime)

(s/def ::record (s/keys :req-un [:record/id :record/state]
                  :opt-un [:record/error,
                           :record/next_id, :record/parent_run_id,
                           :record/response, :record/result, :record/return_mode, :record/run_response,
                           :record/state, :record/stack, :record/start_form,
                           :record/suspend, :record/suspend_expires, :record/suspend_permit
                           :record/updated_at, :record/created_at]))
(defn make-run
  ([] (make-run {}))

  ([{:keys [id, stack, state, response, run-response, result]
     :or   {id           (UUID/randomUUID)
            state        :created
            stack        ()
            response     []
            run-response []}
     :as   fields}]
   {:post [(s/assert ::run %)]}
   (map->Run (into (or fields {})
               {:id           id,
                :state        state,
                :stack        stack,
                :response     response,
                :result       result,
                :run-response run-response}))))

(defn run-to-record
  "Maps a run to a storage record (a hash-map containing a constrained set of values suitable for storage in a database:
  uuids, strings, date-times and binary objects (usually representing complex closure structures))."
  [run]
  {:pre  [(if (run-in-state? run :suspended)
            (-> run :suspend signals/suspend-signal?)
            (-> run :suspend nil?))]
   :post [(s/assert ::record %)]}
  (to-storage-record run
    {:id            identity
     :error         freeze
     :result        freeze
     :next-id       identity
     :parent-run-id identity
     :response      freeze
     :return-mode   key-to-str
     :run-response  freeze
     :start-form    identity
     :state         key-to-str
     :stack         freeze
     :suspend       freeze}
    #{:suspend :stack :next :next-id :return-mode :parent-run-id}))

(defn run-from-record
  "Given a storage record, returns a Run instance"
  [srec]
  {:post [(s/assert ::run %)]}
  (let [fields (from-storage-record srec
                 {:id            identity
                  :state         keyword
                  :start-form    identity
                  :stack         thaw
                  :result        thaw
                  :response      thaw
                  :run-response  thaw
                  :parent-run-id identity
                  :next-id       identity
                  :return-mode   keyword
                  :error         thaw
                  :suspend       thaw}
                 #{:suspend :stack :next :next-id :return-mode :parent-run-id})]
    (map->Run fields)))

(defn contains-some? [m & ks]
  (some #(contains? m %) ks))

(defn make-test-run
  "Returns a run and a record"
  [& remove-keys]
  {:post [(not (contains-some? % remove-keys))]}
  (let [run (make-run                                       ; fill every field of the run
              (apply dissoc {:state         :suspended
                             :start-form    (str `(foo :a 1))
                             :stack         (list (sf/make-stack-frame (a/create `foo 1 2) {:b 2} 'data-key))
                             :suspend       (signals/make-suspend-signal :foo (t/local-date-time) {:a 1})
                             :run-response  ["hello" "there"]
                             :response      [:hello :there]
                             :result        {:data "some-result"}
                             :return-mode   :redirect
                             :parent-run-id (UUID/randomUUID)
                             :next-id       (UUID/randomUUID)
                             :error         (Exception. "foo")}
                remove-keys))
        record (run-to-record run)]
    [run record]))