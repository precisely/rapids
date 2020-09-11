; setup according to: https://8thlight.com/blog/eric-smith/2016/10/05/a-testable-clojurescript-setup.html

; also https://github.com/bhauman/lein-figwheel/wiki/Running-figwheel-in-a-Cursive-Clojure-REPL
(defproject longterm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  ; :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.773"]
                 [org.clojure/core.async "1.3.610"]
                 [figwheel-sidecar "0.5.20"]]
  ; :profiles {:repl {:dependencies {}}}
  :figwheel {:open-file-command "bin/idea-open"}
  :clean-targets ^{:protect false} ["target"]
  :plugins [[lein-cljsbuild "1.1.8" :exclusions [[org.clojure/clojure]]]
            [lein-figwheel "0.5.20"]]
  :profiles {:dev {:source-paths ["scripts" "src"]} }
  :cljsbuild {
              :builds [{:id "dev"             ; development configuration
                        :source-paths ["src"] ; Paths to monitor for build
                        :figwheel true        ; Enable Figwheel
                        :compiler {:main longterm    ; your main namespace
                                   :asset-path "cljs/out"                       ; Where load-dependent files will go, mind you this one is relative
                                   :output-to "resources/public/cljs/main.js"   ; Where the main file will be built
                                   :output-dir "resources/public/cljs/out"      ; Directory for temporary files
                                   :source-map-timestamp true}                  ; Sourcemaps hurray!
                        }]}
  :repl-options {:init-ns longterm})
