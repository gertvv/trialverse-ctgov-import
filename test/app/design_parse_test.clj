(ns app.design-parse-test
  (:use clojure.test)
  (:use app.design-parse))

(deftest test-masking-parser-double-blind
  (is (= (parse-masking "Double Blind (Participant, Investigator, Outcomes Assessor)")
         ["Double Blind" "Participant" "Investigator", "Outcomes Assessor"]  )))

(deftest test-masking-parser-single-blind-single-spec
  (is (= (parse-masking "Single Blind (Outcomes Assessor)")
         ["Single Blind" "Outcomes Assessor"])))

; tests for old design parser
(deftest example1
  (is (= (parse "Allocation: Randomized, Intervention Model: Parallel Assignment, Masking: Double Blind (Subject, Caregiver, Investigator, Outcomes Assessor), Primary Purpose: Treatment")
         [:design
          [:keyval [:key "Allocation"] [:val "Randomized"]]
          [:keyval [:key "Intervention Model"] [:val "Parallel Assignment"]]
          [:keyval [:key "Masking"] [:val "Double Blind" "Subject" "Caregiver" "Investigator" "Outcomes Assessor"]]
          [:keyval [:key "Primary Purpose"] [:val "Treatment"]]]))
  (is (= (design-as-map "Allocation: Randomized, Intervention Model: Parallel Assignment, Masking: Double Blind (Subject, Caregiver, Investigator, Outcomes Assessor), Primary Purpose: Treatment")
         {"Allocation" ["Randomized"]
          "Intervention Model" ["Parallel Assignment"]
          "Masking" ["Double Blind" "Subject" "Caregiver" "Investigator" "Outcomes Assessor"]
          "Primary Purpose" ["Treatment"]})))

(deftest example2
  (is (= (parse "Observational Model: Cohort, Time Perspective: Prospective")
         [:design
           [:keyval [:key "Observational Model"] [:val "Cohort"]]
           [:keyval [:key "Time Perspective"] [:val "Prospective"]]])))

(deftest example3
  (is (= (parse "Allocation: Randomized, Endpoint Classification: Efficacy Study, Intervention Model: Parallel Assignment, Masking: Single Blind (Outcomes Assessor), Primary Purpose: Supportive Care")
        [:design
          [:keyval [:key "Allocation"] [:val "Randomized"]]
          [:keyval [:key "Endpoint Classification"] [:val "Efficacy Study"]]
          [:keyval [:key "Intervention Model"] [:val "Parallel Assignment"]]
          [:keyval [:key "Masking"] [:val "Single Blind" "Outcomes Assessor"]]
          [:keyval [:key "Primary Purpose"] [:val "Supportive Care"]]])))

(deftest example4
  (is (= (parse "Allocation: Randomized, Endpoint Classification: Efficacy Study, Intervention Model: Parallel Assignment, Masking: Open Label, Primary Purpose: Supportive Care")
      [:design
        [:keyval [:key "Allocation"] [:val "Randomized"]]
        [:keyval [:key "Endpoint Classification"] [:val "Efficacy Study"]]
        [:keyval [:key "Intervention Model"] [:val "Parallel Assignment"]]
        [:keyval [:key "Masking"] [:val "Open Label"]]
        [:keyval [:key "Primary Purpose"] [:val "Supportive Care"]]])))

(deftest example5
  (is (= (parse "Allocation: Randomized, Endpoint Classification: Efficacy Study, Intervention Model: Factorial Assignment, Masking: Single Blind (Outcomes Assessor), Primary Purpose: Treatment")
        [:design
          [:keyval [:key "Allocation"] [:val "Randomized"]]
          [:keyval [:key "Endpoint Classification"] [:val "Efficacy Study"]]
          [:keyval [:key "Intervention Model"] [:val "Factorial Assignment"]]
          [:keyval [:key "Masking"] [:val "Single Blind" "Outcomes Assessor"]]
          [:keyval [:key "Primary Purpose"] [:val "Treatment"]]])))

(deftest example6
  (is (= (design-as-map "Allocation: Randomized, Endpoint Classification: Safety/Efficacy Study, Intervention Model: Parallel Assignment, Masking: Double Blind (Subject, Caregiver, Investigator), Primary Purpose: Treatment")
         {"Allocation" ["Randomized"]
          "Endpoint Classification" ["Safety/Efficacy Study"]
          "Intervention Model" ["Parallel Assignment"]
          "Masking" ["Double Blind" "Subject" "Caregiver" "Investigator"]
          "Primary Purpose" ["Treatment"]})))
