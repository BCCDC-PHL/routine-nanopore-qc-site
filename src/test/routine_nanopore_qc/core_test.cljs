(ns routine-nanopore-qc.core-test
  (:require [cljs.test :refer [deftest is testing]]
            [routine-nanopore-qc.core :as core]))


(deftest null-test
  (testing "Placeholder. Should always pass."
    (is (= 1 1))))
