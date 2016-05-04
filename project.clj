(defproject ctgov-import "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "GNU GPL-3"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[clj-http "2.1.0"]
                 [org.clojure/clojure "1.5.1"]
                 [riveted "0.0.9"]
                 [instaparse "1.4.1"]
                 [addis-rdf "0.1.0-SNAPSHOT"]
                 [compojure "1.3.1"]
                 [ring/ring-defaults "0.1.2"]
                 [org.clojure/data.json "0.2.6"]]
  :plugins [[lein-ring "0.8.13"]]
  :ring {:handler app.handler/app}
  :main app.core
  :profiles {:uberjar {:aot :all}})
