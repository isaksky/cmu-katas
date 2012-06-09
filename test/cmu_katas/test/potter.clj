(ns cmu-katas.test.potter
  (:use [cmu-katas.potter])
  (:use [clojure.test]))

(deftest price-test
  (is (= 8.0 (price [1])))
  (is (= 15.2 (price [1 2])))
  (is (= 16.0 (price [3 3]))))

(deftest subtract-from-frequencies-map-test
  (is (= {2 2} (subtract-from-frequencies-map {1 1, 2 3} [1 2]))))

(deftest solve-test
  (are [price books]
       (= price                                 (combos-price (best-bundling books)))
       (+ 8 (* 8 2 0.95))                       {1 2 2 1}
       (* 2 (* 8 4 0.8))                        {1 2, 2 2, 3 2, 4 1, 5 1}
       (+ (* 3 (* 8 5 0.75)) (* 2 (* 8 4 0.8))) {1 5, 2 5, 3 4, 4 5, 5 4}))
