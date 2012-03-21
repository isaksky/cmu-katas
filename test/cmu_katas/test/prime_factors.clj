(ns cmu-katas.test.prime-factors
  (:use [cmu-katas.prime-factors])
  (:use [clojure.test]))

(deftest prime-factors-test
  (is (= (prime-factors 4) [2 2])))

(deftest product-of-factors-is-n
  (doseq [n (range 4 999)]
    (do (is (= n (reduce * (prime-factors-iterative n))))
        (is (= n (reduce * (prime-factors n)))))))


