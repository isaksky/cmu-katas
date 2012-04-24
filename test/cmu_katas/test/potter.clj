(ns cmu-katas.test.potter
  (:use [cmu-katas.potter])
  (:use [clojure.test]))

(deftest price-test
  (is (= 8.0 (price [1])))
  (is (= 15.2 (price [1 2])))
  (is (= 16.0 (price [3 3]))))

(deftest list-subtract-test
  (is (= [1 3] (list-subtract [1 1 3] [1 2]))))

(deftest solve-test
  (are [price books]
       (= price                                 (combos-price (solve books)))
       (+ 8 (* 8 2 0.95))                       [1 1 2]
       (* 2 (* 8 4 0.8))                        (book-set [2 2 2 1 1])
       (+ (* 3 (* 8 5 0.75)) (* 2 (* 8 4 0.8))) (book-set [5 5 4 5 4])))
