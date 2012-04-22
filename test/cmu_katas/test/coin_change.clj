(ns cmu-katas.test.coin-change
  (:use [cmu-katas.coin-change])
  (:use [clojure.test]))

(def normal-coins [25 10 5 1])

(def freaky-coins [25 10 7 5 1])

(deftest change-test
  (is (= [10 5] (change 15 normal-coins)))
  (is (= [25 10 5] (change 40 normal-coins)))
  (is (= [7 7] (change 14 freaky-coins)))
  (is (= [25 7 7] (change 39 freaky-coins))))

(deftest change-adds-up-test
  (doseq [amt (range 1 201)]
    (is (= amt (reduce + (change amt normal-coins))))))