;;; XXX: Dirty workaround to allow referencing environment variables in
;;; project.clj. This approach echoes the envvar library (and, indeed,
;;; reimplements it). See https://github.com/gcv/envvar. This is necessary
;;; because Leiningen middleware is resolved too late to use environment
;;; variables in some places in the project map. Specifically, repository
;;; resolution happens too early.
;;;
;;; Note that Leiningen's built-in repository credentials mechanism is not used
;;; here because GPG setup for credentials files is inconsistent with how all
;;; other credentials to third-party services are stored.

(require '[clojure.java.io :as io]
  '[clojure.string :as str])

(def env
  (->> (merge
         (into {} (System/getenv))
         (into {} (System/getProperties))
         (let [env-file (io/file ".env")]
           (if (.exists env-file)
             (let [props (java.util.Properties.)]
               (.load props (io/input-stream env-file))
               props)
             {})))
    (map (fn [[k v]] [(-> (str/lower-case k)
                        (str/replace "_" "-")
                        (str/replace "." "-")
                        (keyword))
                      v]))
    (into {})))

(defproject precisely/rapids "0.6.0"
  :description "A Clojure DSL for scripting user flows"
  :url "https://github.com/precisely/rapids"
  :license {:name "All Rights Reserved"
            :url  "https://precise.ly/rapids"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/tools.macro "0.1.2"]
                 [org.clojure/data.codec "0.1.1"]
                 [com.taoensso/nippy "3.1.0"]
                 [com.taoensso/timbre "5.1.0"]
                 [com.fzakaria/slf4j-timbre "0.3.21"]       ; needed by next.jdbc
                 [potemkin "0.4.5"]

                 ;; database
                 [org.postgresql/postgresql "42.2.18"]
                 [seancorfield/next.jdbc "1.1.613"]
                 [com.github.seancorfield/honeysql "2.0.783"]
                 [danlentz/clj-uuid "0.1.9"]
                 [migratus "1.3.5"]
                 [hikari-cp "2.13.0"]]
  :aot [rapids.storage.CacheProxy rapids.objects.CurrentContinuationChange]
  :clean-targets ^{:protect false} ["target"]
  :plugins [[s3-wagon-private "1.3.4"]]
  :profiles {:dev
                        {
                         :source-paths ["src"]
                         :dependencies [[expectations/clojure-test "1.2.1"]
                                        [org.clojure/core.match "1.0.0"]
                                        [philoskim/debux "0.8.1"]
                                        [tortue/spy "2.9.0"]
                                        [org.clojars.justiniac/matchure "0.13.1"]
                                        [org.clojure/tools.namespace "1.1.0"]]
                         :plugins      [[migratus-lein "0.7.3"]
                                        [lein-localrepo "0.5.4"]]
                         }

             :cloverage {:plugins   [[lein-cloverage "1.2.2"]]
                         :cloverage {:ns-exclude-regex [#"rapids\.runtime\.cc"]
                                     :test-ns-regex [#"^((?!rapids-interruptions-test).)*$"]}}}
  :repl-options {:init-ns rapids}

  :deploy-repositories [["precisely" {:url           "s3p://precisely-maven-repo/"
                                      :username      ~(env :maven-repo-aws-access-key-id)
                                      :passphrase    ~(env :maven-repo-aws-access-key-secret)
                                      :sign-releases false}]]

  :test-selectors {:default     (complement :integration)
                   :integration :integration
                   :unit        :unit
                   :all         (constantly true)})
