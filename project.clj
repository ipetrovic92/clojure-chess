(defproject clojure-chess "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [hiccup "1.0.5"]
                 [ring/ring-defaults "0.1.2"]
                 [ring/ring-core "1.4.0"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [compojure "1.4.0"]]

:plugins [[lein2-eclipse "2.0.0"]
            [lein-ring "0.8.10"]]

 :ring {:handler clojure-chess.rutes.router/app})