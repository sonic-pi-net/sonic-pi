(defproject defining-pi "0.0.0-SNAPSHOT"
  :description "Sonic Pi HTML Interface"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]]
  :plugins [[lein-cljsbuild "1.0.1"]]
  :source-paths ["src-cljs"]
  :profiles {:dev {:plugins [[com.cemerick/austin "0.1.3"]]}}
  :cljsbuild {
              :builds [{:id "dev"
                        :source-paths ["cljs"]
                        :compiler {
                                   :output-dir "js/generated"
                                   :output-to "js/cljs-main.js"
                                   :source-map "js/cljs-main.js.map"}}
                       {:id "release"
                        :source-paths ["cljs"]
                        :compiler {
                                   :output-to "js/cljs-main.js"
                                   :optimizations :advanced
                                   :pretty-print false}}]})
