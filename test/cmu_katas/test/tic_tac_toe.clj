(ns cmu-katas.test.tic-tac-toe
  (:use [clojure.test])
  (:use [cmu-katas.tic-tac-toe]))

(deftest player-frequencies-test
  (is (= {:a 2 :b 1} (player-frequencies {[0 0] :a [1 1] :a [2 2] :b} [[0 0] [1 1] [2 2]]))))

(def tied-board
  {[0 0] :a [1 0] :b [2 0] :b
   [0 1] :b [1 1] :a [2 1] :a
   [0 2] :a [1 2] :b [2 2] :a})

(def unfinished-board
  (dissoc tied-board [1 1]))

(deftest game-finished?-test
  (is (game-finished? tied-board))
  (is (not (game-finished? unfinished-board))))

(deftest blocked?-test
  (is (blocked? {[0 0] :a [1 0] :b} (first horizontal-coord-lists) :a))
  (is (not (blocked? {[0 0] :a [1 0] :a} (first horizontal-coord-lists) :a))))

(deftest game-result-test
  (is :a (game-result {[0 0] :a [1 0] :a [2 0] :a}))
  (is :b (game-result {[0 0] :b [1 1] :b [2 2] :b}))
  (is nil? (game-result {[0 0] :a [1 0] :a [2 0] :b})))

(deftest empty-coords-test
  (is (= [[1 1]] (empty-coords unfinished-board))))

(def partial-board-1
  {[0 0] :b [1 0] :a})

(def top-horizonal-coord-list
  [[0 0] [1 0] [2 0]])

(def partial-board-2
  {[0 0] :b })

(def partial-board-3
  {[0 0] :b [1 0] :b})

(deftest blocking-score-test
  (is (= 0 (blocking-score partial-board-1 :a top-horizonal-coord-list [2 0])))
  (is (= 1 (blocking-score partial-board-2 :a top-horizonal-coord-list [2 0])))
  (is (= 8 (blocking-score partial-board-3 :a top-horizonal-coord-list [2 0]))))

(deftest win-progress-score-test
  (is (= 0.1 (win-progress-score {} :a top-horizonal-coord-list [2 0])))
  (is (= 1 (win-progress-score partial-board-2 :b top-horizonal-coord-list [2 0])))
  (is (= 32 (win-progress-score partial-board-3 :b top-horizonal-coord-list [2 0]))))

(def partial-board-4
  {[0 0] :a [1 0] :a
   [2 1] :b
   [2 2] :b})

(deftest score-test
  (is (= 40.1 (score partial-board-4 [2 0] :a))))

(def partial-board-5
  {[0 0] :b [1 0] :b
   [1 1] :a})

(deftest best-move-coords-test
  (is (= [2 0] (best-move-coords partial-board-3 :b)))
  (is (= [2 0] (best-move-coords partial-board-5 :a))))

(deftest never-lose-test
  (let [rand-game-results
        (for [i (range 999)]
          (loop [board {}
                 player-turns (cycle [[:a move] [:b rand-move]])
                 all-boards []]
            (cond (game-finished? board)
                  (let [game-result (game-result board)]
                    (if (= :b game-result)
                      (do (println "PROGRAM FAIL! LEARN TO PROGRAM BETTER!")
                          (doseq [board all-boards]
                            (do (print-board board)
                                (println)))))
                    game-result)
                  :else (let [[player move-fn] (first player-turns)
                              new-board (move-fn board player)]
                          (recur new-board (rest player-turns) (conj all-boards new-board))))))]
    ;; Game can be tie, but player B can never win
    (is (= 0 (or (:b (frequencies rand-game-results)) 0)))))
