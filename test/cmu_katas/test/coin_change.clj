(ns cmu-katas.test.coin-change
  (:use [cmu-katas.coin-change])
  (:use [clojure.test]))

(deftest change-test
  (is (= [0 0 1 1 0] (change 15 starting-coins)))
  (is (= [0 1 1 1 0] (change 40 starting-coins))))

(deftest change-adds-up-test
  (doseq [amt (range 1 201)]
    (is (= amt (reduce + (map (fn [num-coins coin] (* num-coins coin))
                              (change amt starting-coins)
                              starting-coins))))))