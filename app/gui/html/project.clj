;;#--
;;# This file is part of Sonic Pi: http://sonic-pi.net
;;# Full project source: https://github.com/samaaron/sonic-pi
;;# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
;;#
;;# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
;;# All rights reserved.
;;#
;;# Permission is granted for use, copying, modification, and
;;# distribution of modified versions of this work as long as this
;;# notice is included.
;;#++

(defproject defining-pi "0.0.0-SNAPSHOT"
  :description "Sonic Pi HTML Interface"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.229"]
                 [org.clojure/core.async "0.2.391"
                  :exclusions [org.clojure/tools.reader]]
                 [com.facebook/react "0.9.0"]
                 [om "0.5.0"]]
  :plugins [[lein-figwheel "0.5.2"]
            [lein-cljsbuild "1.1.4"]]
  :source-paths ["src-cljs"]
  :cljsbuild {
              :builds [{:id "dev"
                        :source-paths ["cljs"]
                        :figwheel {:websocket-host :js-client-host}
                        :compiler {
                                   :output-dir "resources/public/js/generated"
                                   :output-to "resources/public/js/cljs-main.js"
                                   :source-map true
                                   :optimizations :none}}
                       {:id "release"
                        :source-paths ["cljs"]
                        :compiler {
                                   :output-to "resources/public/js/cljs-main.js"
                                   :optimizations :advanced
                                   :pretty-print false}}]}
  :figwheel {:server-ip "0.0.0.0"
             :css-dirs ["resources/public/style"]})
