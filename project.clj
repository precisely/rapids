(defproject longterm "0.1.4-SNAPSHOT"
  :description "FIXME: write description"
  ; :url "http://example.com/FIXME"
  :license {:name "All Rights Reserved"
            :url ""}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.773"]
                 [org.clojure/core.async "1.3.610"]
                 ; [net.cgrand/macrovich "0.2.1"]
                 [potemkin "0.4.5"]]
  ; :profiles {:repl {:dependencies {}}}
  :figwheel {:open-file-command "bin/idea-open"}
  :clean-targets ^{:protect false} ["target"]
  :plugins [[lein-cljsbuild "1.1.8" :exclusions [[org.clojure/clojure]]]
            [lein-figwheel "0.5.20"]]
  :profiles {:dev
    {
      :source-paths ["scripts" "src"]
      :dependencies [[org.clojure/tools.trace "0.7.10"]
                     [org.clojure/core.match "1.0.0"]]
      :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]
                [lein-cloverage "1.1.2"]]
    } }
  :cljsbuild {
              :builds [{:id "dev"             ; development configuration
                        :source-paths ["src"] ; Paths to monitor for build
                        :figwheel true        ; Enable Figwheel
                        :compiler {:main longterm    ; your main namespace
                                   :asset-path "js/out"                       ; Where load-dependent files will go, mind you this one is relative
                                   :output-to "resources/public/js/main.js"   ; Where the main file will be built
                                   :output-dir "resources/public/js/out"      ; Directory for temporary files
                                   :source-map-timestamp true}                  ; Sourcemaps hurray!
                        }]}
  :repl-options {:init-ns longterm}

  :test-selectors {:default (complement :integration)
                   :integration :integration
                   :unit :unit
                   :all (constantly true)})
