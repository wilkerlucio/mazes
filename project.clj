(defproject mazes "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [org.clojure/clojurescript "0.0-3269"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om "0.8.8"]
                 [racehub/om-bootstrap "0.4.2"]]

  :profiles {:dev {:dependencies [[figwheel "0.3.3"]]

                   :plugins [[lein-cljsbuild "1.0.5"]
                             [lein-figwheel "0.3.3"]]}}

  :source-paths ["src/cljs" "src/dev"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src/cljs" "src/dev" "test"]
              :compiler {:output-to "resources/public/js/compiled/mazes.js"
                         :output-dir "resources/public/js/compiled/out"
                         :optimizations :none
                         :main mazes.dev
                         :asset-path "js/compiled/out"
                         :source-map true
                         ;; :source-map-timestamp true
                         :cache-analysis true }}
             {:id "test"
              :source-paths ["src/cljs" "test"]
              :compiler {:output-to "resources/public/js/test/mazes-test.js"
                         :output-dir "resources/public/js/test/out"
                         :optimizations :none
                         :main mazes.test-runner
                         :asset-path "js/test/out"
                         :source-map true
                         ;;: source-map-timestamp true
                         :cache-analysis true }}
             {:id "whitespace"
              :source-paths ["src/cljs"]
              :compiler {:output-to "resources/public/js/white/mazes.js"
                         :main mazes.core
                         :optimizations :whitespace
                         :pretty-print false}}
             {:id "min"
              :source-paths ["src/cljs"]
              :compiler {:output-to "resources/public/js/min/mazes.js"
                         :main mazes.core
                         :optimizations :advanced
                         :pretty-print false}}]}

  :figwheel {
             :http-server-root "public" ;; default and assumes "resources"
             :server-port 3449 ;; default
             :css-dirs ["resources/public/css"] ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process
             :nrepl-port 7888

             ;; Server Ring Handler (optional)
             ;; if you want to embed a ring handler into the figwheel http-kit
             ;; server, this is simple ring servers, if this
             ;; doesn't work for you just run your own server :)
             ;; :ring-handler hello_world.server/handler

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             ;; if you want to disable the REPL
             ;; :repl false

             ;; to configure a different figwheel logfile path
             ;; :server-logfile "tmp/logs/figwheel-logfile.log"
             })
