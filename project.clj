(defn env [x] (eval (System/getenv x)))

(defproject precisely/rapids "0.9.7"
  :description "A Clojure DSL for scripting user flows"
  :url "https://github.com/precisely/rapids"
  :license {:name "All Rights Reserved"
            :url  "https://precise.ly/rapids"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/core.async "1.5.648"]
                 [org.clojure/core.match "1.0.0"]
                 [clojure.java-time "0.3.3"]
                 [org.clojure/data.codec "0.1.1"]
                 [com.taoensso/nippy "3.1.1"]
                 [com.taoensso/timbre "5.1.2"]
                 [com.fzakaria/slf4j-timbre "0.3.21"] ; needed by next.jdbc
                 [potemkin "0.4.5"]

                 ;; database
                 [org.postgresql/postgresql "42.3.3"]
                 [seancorfield/next.jdbc "1.2.659"]
                 [com.github.seancorfield/honeysql "2.2.868"]
                 [migratus "1.3.6"]
                 [hikari-cp "2.13.0"]
                 [metosin/jsonista "0.3.5"]]
  :aot [rapids.storage.CacheProxy rapids.objects.CurrentContinuationChange]
  :clean-targets ^{:protect false} ["target"]
  :profiles {:dev {
                   :source-paths ["src"]
                   :dependencies [[expectations/clojure-test "1.2.1"]
                                  [org.clojure/tools.macro "0.1.5"]
                                  [org.clojure/core.match "1.0.0"]
                                  [philoskim/debux "0.8.2"]
                                  [tortue/spy "2.9.0"]
                                  [org.clojars.justiniac/matchure "0.13.1"]
                                  [org.clojure/tools.namespace "1.2.0"]]
                   :plugins      [[lein-ancient "1.0.0-RC3"]
                                  [com.gearswithingears/shrubbery "0.4.1"] ; protocol spies & stubs
                                  [migratus-lein "0.7.3"]
                                  [lein-cloverage "1.2.2"]
                                  [s3-wagon-private "1.3.4"]]}}
  :repl-options {:init-ns rapids}
  :codox {:doc-paths []}
  :deploy-repositories [["precisely" {:url           "s3p://precisely-maven-repo/"
                                      :username      #=(env "MAVEN_REPO_AWS_ACCESS_KEY_ID")
                                      :passphrase    #=(env "MAVEN_REPO_AWS_ACCESS_KEY_SECRET")
                                      :sign-releases false}]]

  :test-selectors {:default     (complement :integration)
                   :integration :integration
                   :unit        :unit
                   :all         (constantly true)})
