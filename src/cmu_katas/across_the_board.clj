(ns cmu-katas.across-the-board
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as comb]))

;; You are given a grid that contains five words. To discover each word, take one square from each column from left to right. Each square is used only once to form a word.
;; Example:
;; S    A       U       M       ER
;; L    U       F       SE      P
;; SH   Q       I       ST      L
;; SE   R       B       I       D
;; M    O       S       OO      D
;; Output: [Seafood, Squid, Lobster, Shrimp, Mussel]
;; Your program should find all possible unique solutions.

(defn transpose [matrix]
  (apply map vector matrix))

(defn format-input
  "Change input into a form that is easier to work with.
   Add an index to each square, and divide the input into 5
   arrays that represent the columns."
  [input]
  (->> (map (fn [square index] [square index])
            input
            (range (count input)))
       (partition 5)
       transpose))

(defn working-dir-path [path]
  (str (. System getProperty "user.dir") path ))

(def dict-words
  (->> (working-dir-path "/resources/cmudict.0.7a.txt")
       slurp
       (str/split-lines)
       (map str/trim)
       (remove (fn [line] (.startsWith line ";"))) ;; Remove comments
       (map (fn [line] (first (str/split line #"\s"))))
       set))

(defn find-words [input]
  (->> input
       format-input
       (apply comb/cartesian-product)
       (reduce (fn [[square-indexes-used words-found] candidate]
                 (let [candidate-word (str/join (map first candidate))
                       candidate-indexes (map last candidate)]
                   (if (and (not-any? (fn [candidate-index] (contains? square-indexes-used candidate-index))
                                      candidate-indexes)
                            (contains? dict-words candidate-word))
                     [(apply conj square-indexes-used candidate-indexes) (conj words-found candidate-word)]
                     [square-indexes-used words-found])))
               [#{} []])
       last))

;; Example: (find-words test-input-1)
;;          => ["SAFIER" "LOUSED" "SHRIMP"]

(def test-input-1
  ["S" "A" "U" "M" "ER"
   "L" "U" "F" "SE" "P"
   "SH" "Q" "I" "ST" "L"
   "SE" "R" "B" "I" "D"
   "M" "O" "S" "OO" "D"])

(def test-input-2
  ["A" "P" "R" "O" "N"
   "N" "A" "P" "K" "IN"
   "K" "IT" "CH" "E" "N"
   "C" "OO" "K" "I" "NG"
   "B" "A" "K" "I" "NG"])

(def test-input-3
  ["S" "VE" "T" "PI" "ON"
   "M" "A" "RN" "ER" "RE"
   "DE" "E" "AD" "WA" "NG"
   "C" "OF" "L" "EG" "S"
   "L" "E" "LO" "L" "IE"])

;; run all:
;; (map find-words [test-input-1 test-input-2 test-input-3])

(defn ghetto-cartesian-product
  "Built in cartesian-product made it to easy, so here is my own.
   Not using it though, since built in version is much better. (It is iterative and lazy).
   E.g., (ghetto-cartesian-product [[1 2] [3 4]]) ;=> ((1 3) (1 4) (2 3) (2 4))"
  [colls]
  (if (empty? colls)
    [[]]
    (reduce (fn [memo e]
              (concat memo
                      (map (fn [tail] (concat [e] tail))
                           (ghetto-cartesian-product (rest colls)))))
            []
            (first colls))))
