(defproject longterm "0.1.5--SNAPSHOT"
  :description "FIXME: write description"
  ; :url "http://example.com/FIXME"
  :license {:name "All Rights Reserved"
            :url ""}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [potemkin "0.4.5"]]
  :clean-targets ^{:protect false} ["target"]
  :plugins []
  :profiles {:dev
    {
      :source-paths ["src"]
      :dependencies [[org.clojure/core.match "1.0.0"]] ; used by tests
      :plugins [[lein-cloverage "1.1.2"]]
    } }
  :repl-options {:init-ns longterm}

  :test-selectors {:default (complement :integration)
                   :integration :integration
                   :unit :unit
                   :all (constantly true)})
