(defproject ctgov-import "0.1.4"
  :description "Converter from ClinicalTrials.gov XML to ADDIS-compatible RDF (text/turtle)"
  :url "https://github.com/gertvv/trialverse-ctgov-import"
  :license {:name "GNU GPL-3"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[clj-http "2.1.0"]
                 [org.clojure/clojure "1.5.1"]
                 [riveted "0.0.9"]
                 [instaparse "1.4.1"]
                 [org.drugis.addis/rdfexport "1.0.1"]
                 [compojure "1.3.1"]
                 [ring/ring-defaults "0.1.2"]
                 [org.clojure/data.json "0.2.6"]]
  :repositories [["drugis.org" "https://drugis.org/mvn"]]
  :plugins [[lein-ring "0.8.13"]]
  :ring {:handler app.handler/app}
  :main app.core
  :profiles {:uberjar {:aot :all}})
