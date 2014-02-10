(defproject defining-pi "0.0.0-SNAPSHOT"
  :description "Sonic Pi HTML Interface"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138" :scope "provided"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha" :scope "provided"]
                 [com.facebook/react "0.8.0.1"]
                 [om "0.3.6"]]
  :plugins [[lein-cljsbuild "1.0.2"]]
  :source-paths ["src-cljs"]
  :profiles {:dev {:plugins [[com.cemerick/austin "0.1.3"]]}}
  :cljsbuild {
              :builds [{:id "dev"
                        :source-paths ["cljs"]
                        :compiler {
                                   :output-dir "js/generated"
                                   :output-to "js/cljs-main.js"
                                   :source-map true
                                   :optimizations :none}}
                       {:id "release"
                        :source-paths ["cljs"]
                        :compiler {
                                   :output-to "js/cljs-main.js"
                                   :optimizations :advanced
                                   :pretty-print false}}]})
