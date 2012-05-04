(ns cmu-katas.tic-tac-toe)

;; Excercise: Create unbeatable tic-tac-toe

;; Player representation :a or :b

;; Board representation
;; Hash with coords  => :a or :b
;; E.g., {[0 0] :a [1 0] :b  [2 0] :a}

(def horizontal-coord-lists
  (for [y (range 0 3)]
    (for [x (range 0 3)]
      [x y])))

(def vertical-coord-lists
  (for [x (range 0 3)]
    (for [y (range 0 3)]
      [x y])))

(def diagonal-coord-lists
  [[[0 0] [1 1] [2 2]]
   [[0 2] [1 1] [2 0]]])

(def all-winning-coord-lists
  (concat horizontal-coord-lists
          vertical-coord-lists
          diagonal-coord-lists))

(defn player-frequencies [board coord-list]
  (let [freqs (->> coord-list
                   (map #(board %))
                   frequencies)]
    {:a (or (freqs :a) 0)
     :b (or (freqs :b) 0)}))

(def all-coords
  (for [x (range 3) y (range 3)]
    [x y]))

(defn empty-coords [board]
  (remove #(board %) all-coords))

(defn game-result
  "Returns :a if player A won, :b if player B won, or nil if no winner."
  [board]
  (->> all-winning-coord-lists
       (map #(player-frequencies board %))
       (map (fn [player-frequency]
              (cond (= 3 (:a player-frequency)) :a
                    (= 3 (:b player-frequency)) :b)))
       (filter identity)
       first))

(defn game-finished? [board]
  (or (= 0 (count (empty-coords board)))
      (game-result board)))

(defn other-player [player]
  (if (= player :a) :b :a))

(defn blocked? [board coord-list player]
  (let [other-player (other-player player)
        other-player-freqs (other-player (player-frequencies board coord-list))]
    (and other-player-freqs (< 0 other-player-freqs))))

(defn blocking-score [board player coord-list coords]
  (let [player-frequencies (player-frequencies board coord-list)
        other-player (other-player player)]
    (cond
     ;; No points if other player hasn't started on path
     (= 0 (player-frequencies other-player)) 0
     ;; No points if player is already blocked
     (blocked? board coord-list other-player) 0
     ;; Take exponent, because danger does not scale linearly with how many they have
     :else (int (Math/pow (player-frequencies other-player) 3)))))

(defn win-progress-score [board player coord-list coords]
  (cond (blocked? board coord-list player) 0
        :else (let [this-player-frequencies (player (player-frequencies board coord-list))]
                (if (= 0 this-player-frequencies)
                  ;; Give a little bit of points for clear coord-lists. This will make corners score higher
                  ;; than middles, since they are involved in more coord-lists, which is good.
                  0.1
                  ;;Grows faster than block, b/c would rather win than block
                  (int (Math/pow this-player-frequencies 5))))))

(defn score [board coords player]
  (let [related-coord-lists (filter #(some #{coords} %)
                                    all-winning-coord-lists)]
    (reduce + (map (fn [scoring-function]
                     (reduce + (map (fn [coord-list] (scoring-function board player coord-list coords))
                                    related-coord-lists)))
                   [blocking-score win-progress-score]))))

(defn best-move-coords [board player]
  (->> (empty-coords board)
       (sort-by (fn [coords] (score board coords player)))
       last))

(defn move [board player]
  (cond (empty? board) (assoc board [1 1] player)
        (game-finished? board) nil
        :else (let [best-move-coords (best-move-coords board player)]
                (assoc board best-move-coords player))))

(defn rand-move [board player]
  (if-let [empty-coords (empty-coords board)]
    (assoc board (rand-nth empty-coords) player)))

(defn print-board [board]
  (doseq [y (range 3) x (range 3)]
    (if (= 0 x) (print "\n"))
    (print (or (board [x y]) ":_"))))

(defn print-sample-game [against-self?]
  (loop [board {}
         player-turns (cycle [[:a move] [:b (if against-self?
                                              move
                                              rand-move)]])
         all-boards []]
    (cond (game-finished? board) (do (doseq [board all-boards]
                                       (do (print-board board) (println)))
                                     (println "\nWinner: " (game-result board)))
          :else (let [[player move-fn] (first player-turns)
                      new-board (move-fn board player)]
                  (recur new-board (rest player-turns) (conj all-boards new-board))))))