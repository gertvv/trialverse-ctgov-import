(ns app.core
  (:require 
    [clojure.java.io :refer [as-file]]
    [clojure.string :refer [lower-case]]
    [clojure.set :refer [map-invert]]
    [riveted.core :as vtd]
    [org.drugis.addis.rdf.trig :as trig]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn spo-each [subj pred obj*]
  (reduce (fn [subj obj] (trig/spo subj [pred obj])) subj obj*))

; probe the outcome for measurement properties
(defn outcome-measurement-properties
  [xml]
  (let [measures-xml (vtd/at xml "./measure_list")
        measure-count (count (vtd/children measures-xml))
        measure-xml (vtd/last-child measures-xml)
        categories-xml (vtd/at measure-xml "./category_list")
        category-count (count (vtd/children categories-xml))
        category-xml (vtd/first-child categories-xml)
        ; probe the measure for type: <param> and <dispersion>, plus <units>
        param (vtd/text (vtd/at measure-xml "./param"))
        dispersion (vtd/text (vtd/at measure-xml "./dispersion"))
        units (vtd/text (vtd/at measure-xml "./units"))]
      { :simple (= 1 category-count)
        :param param
        :dispersion dispersion
        :units units
        :unit-of-analysis (= 3 measure-count) }))

; determine results properties from the measurement properties
(defn outcome-results-properties
  [props]
  (concat
    (if (= "Mean" (:param props)) {"mean" "value"})
    (if (= "Median" (:param props)) {"median" "value"})
    (if (= "Number" (:param props)) {"count" "value"}) ; FIXME: or percentage
    (if (= "Geometric Mean" (:param props)) {"geometric_mean" "value"}) ; FIXME: ad to ontology?
    (if (= "Log Mean" (:param props)) {"log_mean" "value"}) ; FIXME: add to ontology?
    (if (= "Least Squares Mean" (:param props)) {"least_squares_mean" "value"}) ; FIXME: add to ontology?
    (if (= "90% Confidence Interval" (:dispersion props)) {"quantile_0.05" "lower_limit"
                                                           "quantile_0.95" "upper_limit"})
    (if (= "95% Confidence Interval" (:dispersion props)) {"quantile_0.025" "lower_limit"
                                                           "quantile_0.975" "upper_limit"})
    (if (= "Full Range" (:dispersion props)) {"min" "lower_limit"
                                              "max" "upper_limit"})
    (if (= "Geometric Coefficient of Variation" (:dispersion props)) {"geometric_coefficient_of_variation" "spread"}) ; FIXME: add to ontology?
    (if (= "Inter-Quartile Range" (:dispersion props)) {"first_quartile" "lower_limit"
                                                        "third_quartile" "upper_limit"})
    (if (= "Standard Deviation" (:dispersion props)) {"standard_deviation" "spread"})
    (if (= "Standard Error" (:dispersion props)) {"standard_error" "spread"})))

(defn outcome-measurement-type
  [props]
  (if (= "Number" (:param props)) "dichotomous" "continuous"))

(defn outcome-rdf
  [xml idx outcome-uris mm-uris]
  (let [uri (outcome-uris [:outcome idx])
        props (outcome-measurement-properties xml)
        properties (outcome-results-properties props)]
    (spo-each
      (trig/spo uri
                [(trig/iri :rdf "type") (trig/iri :ontology "Endpoint")]
                [(trig/iri :rdfs "label") (trig/lit (vtd/text (vtd/at xml "./title")))]
                [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "./description")))]
                [(trig/iri :ontology "is_measured_at") (mm-uris [:outcome idx])]
                [(trig/iri :ontology "has_result_property") (trig/iri :ontology "sample_size")]
                [(trig/iri :ontology "of_variable")
                 (trig/_po [(trig/iri :ontology "measurementType") (trig/iri :ontology (outcome-measurement-type props))])])
      (trig/iri :ontology "has_result_property")
      (map #(trig/iri :ontology %) (keys properties)))))

(defn group-rdf
  [group-uri group-info]
  (trig/spo group-uri
            [(trig/iri :rdfs "label") (trig/lit (:title group-info))]
            [(trig/iri :rdfs "comment") (trig/lit (:description group-info))]
            [(trig/iri :rdf "type") (trig/iri :ontology "Arm")])) ; FIXME


(defn find-arm-groups
  [xml]
  (into {} (map (fn [ag idx] [[:arm_group idx]
                              {:title (vtd/text (vtd/at ag "./arm_group_label"))
                               :description (or (vtd/text (vtd/at ag "./description")) "")}])
                (vtd/search xml "/clinical_study/arm_group")
                (iterate inc 1))))

(defn group-info
  [group-xml]
  {:title (vtd/text (vtd/at group-xml "./title"))
   :description (or (vtd/text (vtd/at group-xml "./description")) "")})

(defn find-baseline-groups
  [xml]
  (into {} (map (fn [group] [[:baseline_group (vtd/attr group :group_id)]
                             (group-info group)])
                (vtd/search xml "/clinical_study/clinical_results/baseline/group_list/group"))))

(defn find-event-groups
  [xml]
  (into {} (map (fn [group] [[:events_group (vtd/attr group :group_id)]
                             (group-info group)])
                (vtd/search xml "/clinical_study/clinical_results/reported_events/group_list/group"))))

(defn find-groups-for-outcome
  [outcome-xml idx]
  (into {} (map (fn [group] [[:outcome_group idx (vtd/attr group :group_id)]
                             (group-info group)])
                (vtd/search outcome-xml "./group_list/group"))))

(defn find-outcome-groups
  [xml]
  (apply merge (map find-groups-for-outcome
                    (vtd/search xml "/clinical_study/clinical_results/outcome_list/outcome")
                    (iterate inc 1))))

(defn std-group
  [group]
  {:title (lower-case (:title group))
   :description (lower-case (:description group))})

(defn assign-uri-to-cluster
  [cluster]
  (let [uri (trig/iri :instance (uuid))]
    (into {} (map #(vector % uri) cluster))))

(defn sort-equivalent-values
  [the-map std-fn]
  (let [clusters (map #(map first %)
                      (vals (group-by #(std-fn (second %)) the-map)))
        uris (apply merge (map assign-uri-to-cluster clusters))
        info (into {} (map #(vector (first %) (the-map (second %))) (map-invert uris)))]
    [uris info]))

(defn find-groups
  [xml]
  (sort-equivalent-values (merge (find-arm-groups xml)
                                 (find-baseline-groups xml)
                                 (find-event-groups xml)
                                 (find-outcome-groups xml))
                          std-group))

(defn mm-rdf
  [mm-uri mm-title]
  (trig/spo mm-uri
            [(trig/iri :rdfs "label") (trig/lit mm-title)]
            [(trig/iri :rdf "type") (trig/iri :ontology "MeasurementMoment")]))

(defn find-event-time-frame
  [xml]
  { [:events] (or (vtd/text (vtd/at xml "/clinical_study/clinical_results/reported_events/time_frame")) "Unknown") })

(defn find-outcome-time-frames
  [xml]
  (into {} (map #(vector [:outcome %2] (vtd/text %1))
                (vtd/search xml "/clinical_study/clinical_results/outcome_list/outcome/time_frame")
                (iterate inc 1))))

(defn find-measurement-moments
  [xml]
  (sort-equivalent-values (merge (find-event-time-frame xml)
                                 (find-outcome-time-frames xml)
                                 {[:baseline] "Baseline"})
                                 lower-case))

(defn measurement-meta-rdf
  [subj xml idx group-id outcome-uris group-uris mm-uris]
  (let [outcome-uri (outcome-uris [:outcome idx])
        group-uri (group-uris [:outcome_group idx group-id])
        mm-uri (mm-uris [:outcome idx])]
    (trig/spo subj
              [(trig/iri :ontology "of_outcome") outcome-uri]
              [(trig/iri :ontology "of_arm") group-uri]
              [(trig/iri :ontology "of_moment") mm-uri])))

(defn parse-int
  [s]
  (try (Integer. s)
       (catch Exception e
         nil)))

(defn parse-double
  [s]
  (try (Double. s)
       (catch Exception e
         nil)))

(defn measurement-value
  [subj xml prop attr]
  (let [value-str (vtd/attr xml attr)
        value (if (or (= "count" prop) (= "sample_size" prop)) (parse-int value-str) (parse-double value-str))]
    (if value
      (trig/spo subj [(trig/iri :ontology prop) (trig/lit value)])
      subj)))

(defn measurement-data-rdf
  [subj xml group-id]
  (let [measures-xml (vtd/at xml "./measure_list")
        measure-count (count (vtd/search measures-xml "./measure"))
        sample-size-xml (if (= 3 measure-count)
                          (vtd/next-sibling (vtd/first-child measures-xml))
                          (vtd/first-child measures-xml))
        measure-xml (vtd/last-child measures-xml)
        categories-xml (vtd/at measure-xml "./category_list")
        category-count (count (vtd/children categories-xml))
        category-xml (vtd/first-child categories-xml)
        measurements-xml (vtd/at category-xml "./measurement_list")
        measurement-query (format "./category_list/category/measurement_list/measurement[@group_id=\"%s\"]" group-id)
        sample-size (vtd/at sample-size-xml measurement-query)
        measurement-xml (vtd/at measurements-xml (format "./measurement[@group_id=\"%s\"]" group-id))
        props (outcome-measurement-properties xml)
        properties (outcome-results-properties props)]
    (if (:simple props)
      (reduce #(measurement-value %1 measurement-xml (first %2) (second %2))
              (measurement-value subj sample-size "sample_size" "value")
              properties)
      subj)))


(defn outcome-measurements
  [xml idx outcome-uris group-uris mm-uris]
  (let [group-id-query "./measure_list/measure/category_list/category/measurement_list/measurement/@group_id"
        groups (set (map vtd/text (vtd/search xml group-id-query)))
        m-meta (into {} (map (fn [g] [g (measurement-meta-rdf (trig/iri :instance (uuid)) xml idx g outcome-uris group-uris mm-uris)]) groups))]
    (map (fn [[g s]] (measurement-data-rdf s xml g)) m-meta)))

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
        [mm-uris mm-info] (find-measurement-moments xml)
        outcome-xml (vtd/search xml "/clinical_study/clinical_results/outcome_list/outcome")
        outcome-uris (into {} (map #(vector [:outcome %2] (trig/iri :instance (uuid))) outcome-xml (iterate inc 1)))
        outcomes-rdf (map #(outcome-rdf %1 %2 outcome-uris mm-uris) outcome-xml (iterate inc 1))
        [group-uris group-info] (find-groups xml)
        groups-rdf (map #(group-rdf (first %) (second %)) group-info)
        mms-rdf (map #(mm-rdf (first %) (second %)) mm-info)
        measurements-rdf (apply concat (map #(outcome-measurements %1 %2 outcome-uris group-uris mm-uris) outcome-xml (iterate inc 1)))
        study-rdf (-> uri
                     (trig/spo [(trig/iri :rdf "type") (trig/iri :ontology "Study")]
                               [(trig/iri :rdfs "label") (trig/lit (vtd/text (vtd/at xml "/clinical_study/brief_title")))]
                               [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/official_title")))])
                     (spo-each (trig/iri :ontology "has_outcome") (vals outcome-uris))
                     (spo-each (trig/iri :ontology "has_arm") (keys group-info)))
        triples (concat [study-rdf] mms-rdf outcomes-rdf groups-rdf measurements-rdf)]
    (trig/write-ttl prefixes triples)))

(defn -main
  [& args]
  (let [data (vtd/navigator (slurp (as-file (first args))))]
    (println (ctgov-import data))))
