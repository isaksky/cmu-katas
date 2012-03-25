(ns cmu-katas.test.game-of-life
  (:use [cmu-katas.game-of-life])
  (:use [clojure.test]))

(deftest num-neighbors-test
  (is (= 0 (num-neighbors [1 1] #{[1 1]})))
  (is (= 1 (num-neighbors [1 1] #{[1 1] [1 2]})))
  (is (= 2 (num-neighbors [1 1] #{[1 1] [1 2] [2 2]})))
  (is (= 3 (num-neighbors [1 1] #{[1 1] [1 2] [2 2] [2 1]}))))

;;; Rules from http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
(deftest tick-test
  (is (= #{} (tick #{[0 1]})) "Any live cell with fewer than two live neighbours dies, as if caused by under-population.")
  (let [board (tick #{[0 1] [0 2] [0 3]})]
    (is (contains? (board [0 2])) "Any live cell with two or three live neighbours lives on to the next generation."))
  (let [board (tick #{[1 0] [2 0] [3 0]
                      [1 1] [2 1]})]
    (is (nil? (board [2 0])) "Any live cell with more than three live neighbours dies, as if by overcrowding."))
  (let [board (tick #{[1 0] [2 0] [3 0]})]
    (is (contains? board [2 1]) "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.")))