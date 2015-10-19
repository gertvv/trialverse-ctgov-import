(ns app.core
  (:require 
    [clojure.java.io :refer [as-file]]
    [clojure.string :refer [lower-case]]
    [clojure.set :refer [map-invert]]
    [app.design-parse :refer [design-as-map]]
    [riveted.core :as vtd]
    [org.drugis.addis.rdf.trig :as trig]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn spo-each [subj pred obj*]
  (reduce (fn [subj obj] (trig/spo subj [pred obj])) subj obj*))

(defn row-label-sample-size
  [label]
  (let [m (re-find #"(?i)\Wn\s*=\s*(\d*)([,;]\s*(\d*))*" label)
        s (map #(Integer/parseInt %) (remove nil? (take-nth 2 (rest m))))]
    s))

(defn measurement-row-info
  [xml row-title]
  (let [group-ids (map #(vtd/attr % "group_id") (vtd/search xml "./group_list/group"))
        sample-size-guess (row-label-sample-size row-title)
        sample-size (if (and sample-size-guess (= (count sample-size-guess) (count group-ids)))
                        (zipmap group-ids sample-size-guess)
                        {})]
    { :title row-title :sample-size sample-size }))

; probe the outcome for measurement properties
(defn outcome-measurement-properties
  [xml]
  (let [measures-xml (vtd/at xml "./measure_list")
        measure-count (count (vtd/children measures-xml))
        measure-xml (vtd/last-child measures-xml)
        categories-xml (vtd/at measure-xml "./category_list")
        category-count (count (vtd/children categories-xml))
        category-info (map #(measurement-row-info xml (vtd/text %)) (vtd/search categories-xml "./category/sub_title"))
        category-xml (vtd/first-child categories-xml)
        ; probe the measure for type: <param> and <dispersion>, plus <units>
        param (vtd/text (vtd/at measure-xml "./param"))
        dispersion (vtd/text (vtd/at measure-xml "./dispersion"))
        units (vtd/text (vtd/at measure-xml "./units"))]
      { :simple (= 1 category-count)
        :categories category-info
        :param param
        :dispersion dispersion
        :units units
        :unit-of-analysis (= 3 measure-count) }))

(defn baseline-measurement-properties
  [measure-xml]
  (let [categories-xml (vtd/at measure-xml "./category_list")
        category-titles (map vtd/text (vtd/search categories-xml "./category/sub_title"))
        param (vtd/text (vtd/at measure-xml "./param"))
        dispersion (vtd/text (vtd/at measure-xml "./dispersion"))
        units (vtd/text (vtd/at measure-xml "./units"))]
    { :categories category-titles
      :simple (empty? category-titles)
      :param param
      :dispersion dispersion
      :units units }))

(defn string-starts-with-any
  [s words]
  (some #(.startsWith s %) words))

(defn is-proportion-outcome
  [props]
  (and (= "Number" (:param props)) (string-starts-with-any (lower-case (:units props)) ["proportion"])))

(defn is-percent-outcome
  [props]
  (and (= "Number" (:param props)) (string-starts-with-any (lower-case (:units props)) ["percent" "percentage" "percentages" "%" "observed percentage"])))

(defn is-count-outcome
  [props]
  (and (= "Number" (:param props)) (not (is-percent-outcome props)) (not (is-proportion-outcome props))))

; determine results properties from the measurement properties
(defn outcome-results-properties
  [props]
  (concat
    (if (= "Mean" (:param props)) {"mean" "value"})
    (if (= "Median" (:param props)) {"median" "value"})
    (if (is-count-outcome props) {"count" "value"})
    (if (is-percent-outcome props) {"percentage" "value"}) ; FIXME: add to ontology?
    (if (is-proportion-outcome props) {"proportion" "value"}) ; FIXME: add to ontology?
    (if (= "Geometric Mean" (:param props)) {"geometric_mean" "value"}) ; FIXME: add to ontology?
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

(defn baseline-var-type
  [props]
  (if (= 0 (count (:categories props)))
    (trig/_po [(trig/iri :ontology "measurementType") (trig/iri :ontology (outcome-measurement-type props))])
    (if (= "Number" (:param props))
      (trig/_po [(trig/iri :ontology "measurementType") (trig/iri :ontology "categorical")]
                [(trig/iri :ontology "categoryList") (trig/coll (map trig/lit (:categories props)))])
      (trig/_po))))

(defn baseline-var-rdf
  [xml idx baseline-uris mm-uris]
  (let [uri (baseline-uris idx)
        var-name (vtd/text (vtd/at xml "./title"))
        props (baseline-measurement-properties xml)
        properties (outcome-results-properties props)
        var-rdf (baseline-var-type props)
        subj (trig/spo uri
                       [(trig/iri :rdf "type") (trig/iri :ontology "PopulationCharacteristic")]
                       [(trig/iri :rdfs "label") (trig/lit var-name)]
                       [(trig/iri :ontology "is_measured_at") (mm-uris [:baseline])]
                       [(trig/iri :ontology "has_result_property") (trig/iri :ontology "sample_size")]
                       [(trig/iri :ontology "of_variable") var-rdf])]
        (if (= 0 (count (:categories props)))
          (spo-each subj
                    (trig/iri :ontology "has_result_property")
                    (map #(trig/iri :ontology %) (keys properties)))
          subj)))

(defn adverse-event-rdf
  [xml idx event-uris mm-uris]
  (let [uri (event-uris idx)
        event-name (vtd/text (vtd/at xml "./sub_title"))
        group-name (vtd/text (vtd/at xml "../../title"))]
    (spo-each
      (trig/spo uri
                [(trig/iri :rdf "type") (trig/iri :ontology "AdverseEvent")]
                [(trig/iri :rdfs "label") (trig/lit event-name)]
                [(trig/iri :rdfs "comment") (trig/lit (str group-name ", " event-name))]
                [(trig/iri :ontology "is_measured_at") (mm-uris [:events])]
                [(trig/iri :ontology "of_variable")
                 (trig/_po [(trig/iri :ontology "measurementType") (trig/iri :ontology "dichotomous")])])
      (trig/iri :ontology "has_result_property")
      (map #(trig/iri :ontology %) ["sample_size" "count" "event_count"]))))

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
  [subj outcome-uri group-uri mm-uri]
  (trig/spo subj
            [(trig/iri :ontology "of_outcome") outcome-uri]
            [(trig/iri :ontology "of_arm") group-uri]
            [(trig/iri :ontology "of_moment") mm-uri]))

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
        value (if (or (= "count" prop) (= "event_count" prop) (= "sample_size" prop)) (parse-int value-str) (parse-double value-str))]
    (if value
      (trig/spo subj [(trig/iri :ontology prop) (trig/lit value)])
      subj)))

;    ontology:category_count [
;      ontology:category "Female" ;
;      ontology:count "43"
;    ] .


(defn measurement-data-rdf-basic
  [subj properties sample-size-xml measure-xml group-id]
  (let [measurement-query (format "./category_list/category/measurement_list/measurement[@group_id=\"%s\"]" group-id)
        sample-size (vtd/at sample-size-xml measurement-query)
        measurement-xml (vtd/at measure-xml measurement-query)]
    (reduce #(measurement-value %1 measurement-xml (first %2) (second %2))
            (measurement-value subj sample-size "sample_size" "value")
            properties)))

(defn measurement-data-rdf-categorical
  [subj measure-xml group-id]
  (let [categories-xml (vtd/search measure-xml "./category_list/category")
        measurement-query (format "./measurement_list/measurement[@group_id=\"%s\"]" group-id)]
    (reduce #(trig/spo %1 [(trig/iri :ontology "category_count")
                           (trig/_po [(trig/iri :ontology "category") (vtd/text (vtd/at %2 "./sub_title"))]
                                     [(trig/iri :ontology "count") (vtd/attr (vtd/at %2 measurement-query) :value)])])
            subj
            categories-xml)))

(defn measurement-data-row-rdf
  [measure-xml group-id m-meta props row-info sample-size-xml]
  (let [measurement-xml (vtd/at measure-xml (format "./category_list/category/sub_title[text()=\"%s\"]/../measurement_list/measurement[@group_id=\"%s\"]" (:title row-info) group-id))
        subj (trig/spo (trig/iri :instance (uuid))
                       [(trig/iri :ontology "of_outcome") (:outcome m-meta)]
                       [(trig/iri :ontology "of_arm") (:group m-meta)]
                       [(trig/iri :rdfs "comment") (trig/lit (:title row-info))])
        row-specific ((:sample-size row-info) group-id)
        subj-with-sample-size (if (nil? row-specific)
                                  (measurement-value subj sample-size-xml "sample_size" "value")
                                  (trig/spo subj [(trig/iri :ontology "sample_size") row-specific])) 
        properties (outcome-results-properties props)]
    (reduce #(measurement-value %1 measurement-xml (first %2) (second %2))
            subj-with-sample-size
            properties)))

(defn measurement-data-rdf-complex
  [props sample-size-xml measure-xml group-id m-meta]
  (let [measurement-query (format "./category_list/category/measurement_list/measurement[@group_id=\"%s\"]" group-id)
        sample-size (vtd/at sample-size-xml measurement-query)]
  (map #(measurement-data-row-rdf measure-xml group-id m-meta props % sample-size) (:categories props))))

(defn outcome-measurement-data-rdf
  [xml group-id m-meta]
  (let [measures-xml (vtd/at xml "./measure_list")
        measure-count (count (vtd/search measures-xml "./measure"))
        sample-size-xml (if (= 3 measure-count)
                          (vtd/next-sibling (vtd/first-child measures-xml))
                          (vtd/first-child measures-xml))
        measure-xml (vtd/last-child measures-xml)
        props (outcome-measurement-properties xml)
        properties (outcome-results-properties props)]
    (if (:simple props)
      [(measurement-data-rdf-basic
         (measurement-meta-rdf (trig/iri :instance (uuid)) (:outcome m-meta) (:group m-meta) (:mm m-meta))
         properties sample-size-xml measure-xml group-id)]
      (measurement-data-rdf-complex props sample-size-xml measure-xml group-id m-meta))))

(defn outcome-measurements
  [xml idx outcome-uris group-uris mm-uris]
  (let [group-id-query "./measure_list/measure/category_list/category/measurement_list/measurement/@group_id"
        groups (set (map vtd/text (vtd/search xml group-id-query)))
        m-meta (into {} (map (fn [g] [g { :outcome (outcome-uris [:outcome idx])
                                          :group (group-uris [:outcome_group idx g])
                                          :mm (mm-uris [:outcome idx]) }]) groups))]
    (apply concat (map (fn [[g m]] (outcome-measurement-data-rdf xml g m)) m-meta))))

(defn baseline-measurement-data-rdf
  [subj measure-xml sample-size-xml group-id]
  (let [props (baseline-measurement-properties measure-xml)
        properties (outcome-results-properties props)]
    (cond
      (:simple props) (measurement-data-rdf-basic subj properties sample-size-xml measure-xml group-id)
      (:categories props) (measurement-data-rdf-categorical subj measure-xml group-id)
      :else subj)))

(defn baseline-measurements
  [xml idx sample-size-xml baseline-uris group-uris mm-uris]
  (let [group-id-query "./category_list/category/measurement_list/measurement/@group_id"
        groups (set (map vtd/text (vtd/search xml group-id-query)))
        m-meta (into {} (map (fn [g] [g (measurement-meta-rdf (trig/iri :instance (uuid))
                                                              (baseline-uris idx)
                                                              (group-uris [:baseline_group g])
                                                              (mm-uris [:baseline]))]) groups))]
    (map (fn [[g s]] (baseline-measurement-data-rdf s xml sample-size-xml g)) m-meta)))

(defn event-measurement-rdf
  [xml event-uri group-uri mm-uri]
  (let [m-meta (trig/spo (trig/iri :instance (uuid))
                         [(trig/iri :ontology "of_outcome") event-uri]
                         [(trig/iri :ontology "of_arm") group-uri]
                         [(trig/iri :ontology "of_moment") mm-uri])
        properties {"count" "subjects_affected"
                    "event_count" "events"
                    "sample_size" "subjects_at_risk"}]
    (reduce #(measurement-value %1 xml (first %2) (second %2)) m-meta properties)))

(defn event-measurements
  [xml idx event-uris group-uris mm-uris]
  (let [group-id-query "./counts/@group_id"
        groups (set (map vtd/text (vtd/search xml group-id-query)))]
    (map #(event-measurement-rdf
            (vtd/at xml (format "./counts[@group_id=\"%s\"]" %))
            (event-uris idx)
            (group-uris [:events_group %])
            (mm-uris [:events])) groups)))

(defn allocation-rdf [subj allocation]
  (if allocation
    (trig/spo subj [(trig/iri :ontology "has_allocation")
                    (if (= "Randomized" (first allocation))
                      (trig/iri :ontology "AllocationRandomized")
                      (trig/iri :ontology "AllocationNonRandomized"))])
    subj))


(defn blinding-rdf [subj blinding]
  (if blinding
    (trig/spo subj [(trig/iri :ontology "has_blinding")
                    (trig/iri :ontology (clojure.string/replace (first blinding) " " ""))])
    subj))

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
        event-xml (vtd/search xml "/clinical_study/clinical_results/reported_events/*/category_list/category/event_list/event")
        event-uris (into {} (map #(vector %2 (trig/iri :instance (uuid))) event-xml (iterate inc 1)))
        events-rdf (map #(adverse-event-rdf %1 %2 event-uris mm-uris) event-xml (iterate inc 1))
        baseline-xml (vtd/search xml "/clinical_study/clinical_results/baseline/measure_list/measure")
        baseline-sample-size-xml (first baseline-xml)
        baseline-var-xml (rest baseline-xml)
        baseline-uris (into {} (map #(vector %2 (trig/iri :instance (uuid))) baseline-var-xml (iterate inc 1)))
        baseline-rdf (map #(baseline-var-rdf %1 %2 baseline-uris mm-uris) baseline-var-xml (iterate inc 1))
        [group-uris group-info] (find-groups xml)
        groups-rdf (map #(group-rdf (first %) (second %)) group-info)
        mms-rdf (map #(mm-rdf (first %) (second %)) mm-info)
        measurements-rdf (concat
                           (apply concat (map #(baseline-measurements %1 %2 baseline-sample-size-xml baseline-uris group-uris mm-uris) baseline-var-xml (iterate inc 1)))
                           (apply concat (map #(outcome-measurements %1 %2 outcome-uris group-uris mm-uris) outcome-xml (iterate inc 1)))
                           (apply concat (map #(event-measurements %1 %2 event-uris group-uris mm-uris) event-xml (iterate inc 1))))
        design (design-as-map (vtd/text (vtd/at xml "/clinical_study/study_design")))
        study-rdf (-> uri
                     (trig/spo [(trig/iri :rdf "type") (trig/iri :ontology "Study")]
                               [(trig/iri :rdfs "label") (trig/lit (vtd/text (vtd/at xml "/clinical_study/brief_title")))]
                               [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/official_title")))]
                               [(trig/iri :ontology "has_objective")
                                (trig/_po [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/brief_summary/textblock")))])]
                               [(trig/iri :ontology "has_eligibility_criteria")
                                (trig/_po [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/eligibility/criteria/textblock")))])])

                     (allocation-rdf (design "Allocation"))
                     (blinding-rdf (design "Masking"))
                     (spo-each (trig/iri :ontology "has_outcome") (vals outcome-uris))
                     (spo-each (trig/iri :ontology "has_arm") (keys group-info)))
        triples (concat [study-rdf] mms-rdf baseline-rdf outcomes-rdf events-rdf groups-rdf measurements-rdf)]
    (trig/write-ttl prefixes triples)))

(defn -main
  [& args]
  (let [data (vtd/navigator (slurp (as-file (first args))))]
    (println (ctgov-import data))))
