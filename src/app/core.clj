(ns app.core
  (:require 
    [clojure.java.io :refer [as-file]]
    [riveted.core :as vtd]
    [org.drugis.addis.rdf.trig :as trig]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn spo-each [subj pred obj*]
  (reduce (fn [subj obj] (trig/spo subj [pred obj])) subj obj*))

(defn outcome-rdf
  [xml]
  (let [uri (trig/iri :instance (uuid))]
    [uri (trig/spo uri
                   [(trig/iri :rdf "type") (trig/iri :ontology "Endpoint")]
                   [(trig/iri :rdfs "label") (trig/lit (vtd/text (vtd/at xml "./title")))])]))


(defn ctgov-import
  [xml]
  (let [prefixes {:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                  :rdfs "http://www.w3.org/2000/01/rdf-schema#"
                  :xsd "http://www.w3.org/2001/XMLSchema#"
                  :owl "http://www.w3.org/2002/07/owl#"
                  :qudt "http://qudt.org/schema/qudt#"
                  :ontology "http://trials.drugis.org/ontology#"
                  :study "http://trials.drugis.org/studies/"
                  :instance "http://trials.drugis.org/instances/"
                  :entity "http://trials.drugis.org/entities/"
                  :dc "http://purl.org/dc/elements/1.1/"}
        uri (trig/iri :study (vtd/text (vtd/at xml "/clinical_study/id_info/nct_id")))
        outcome-xml (vtd/search xml "/clinical_study/clinical_results/outcome_list/outcome")
        outcomes (into {} (map outcome-rdf outcome-xml))
        outcomes-rdf (vals outcomes)
        outcomes-uri (keys outcomes)
        study-rdf (-> uri
                     (trig/spo [(trig/iri :rdf "type") (trig/iri :ontology "Study")]
                               [(trig/iri :rdfs "label") (trig/lit (vtd/text (vtd/at xml "/clinical_study/brief_title")))]
                               [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/official_title")))])
                     (spo-each (trig/iri :ontology "has_outcome") outcomes-uri))
        triples (cons study-rdf outcomes-rdf)]
    (trig/write-ttl prefixes triples)))

(defn -main
  [& args]
  (let [data (vtd/navigator (slurp (as-file (first args))))]
    (println (ctgov-import data))))
