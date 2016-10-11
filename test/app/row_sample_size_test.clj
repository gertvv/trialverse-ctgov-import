(ns app.row-sample-size-test
  (:use clojure.test)
  (:use app.core))

(deftest example-no-sample-size
  (is (= (row-label-sample-size "Physical Component at baseline (week 0)") [])))

(deftest example-one-sample-size
  (is (= (row-label-sample-size "Tmax (0 to 8 hours), n=27") [27])))

(deftest example-two-sample-sizes
  (is (= (row-label-sample-size "Change at 4 weeks (n=368, 371)") [368 371])))

(deftest example-three-sample-sizes
  (is (= (row-label-sample-size "Change at 4 weeks (n=368, 371, 367)") [368 371 367])))

(deftest example-alternative-format
  (is (= (row-label-sample-size "Week 2 (n=343; n=334; n=345)") [343 334 345])))
