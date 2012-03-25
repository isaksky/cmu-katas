(ns cmu-katas.game-of-life)

(defn num-neighbors [cell board]
  (let [[x y] cell]
    (count (filter (fn [cell] (board cell))
                   (disj (into #{} (for [x (range (dec x) (+ 2 x))
                                         y (range (dec y) (+ 2 y))]
                                     [x y]))
                         cell)))))

(def max-board-size 10)

(defn tick [board]
  (reduce
   (fn [new-board cell]
     (let [alive? (contains? board cell)
           num-neighbors (num-neighbors cell board)]
       (cond (or (and alive? (<= 2 num-neighbors 3))
                 (and (not alive?) (= 3 num-neighbors)))
             (conj new-board cell)
             :else new-board)))
   #{}
   (for [x (range max-board-size) y (range max-board-size)] [x y])))

(defn print-board [board]
  (doseq [y (range 0 max-board-size)
          x (range 0 max-board-size)]
    (let [cell-alive? (contains? board [x y])]
      (if cell-alive?
        (print "O ")
        (print ". ")))
    (when (= x (dec max-board-size))
      (print "\n"))))

(def glider #{[1 0]
              [2 1]
              [0 2] [1 2] [2 2]})

(defn print-and-advance-board [board ticks]
  (doseq [board (take ticks (iterate tick board))]
    (print-board board)
    (Thread/sleep 500)))

;; For example:
;; (print-and-advance-board glider 10)