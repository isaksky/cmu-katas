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

(defn find-words [columns]
  (->> columns
       (apply comb/cartesian-product)
       (map str/join)
       (filter #(dict-words %))))

;; Example
;; (find-words test-cols)
;; Result: 
;;("SAFIER" "SQUIER" "SQUID" "SQUID" "SOBIL" "LOUSED" "LOUSED"
;; "LOISEL" "LOBSTER" "SHRIMP" "SEAFOOD" "SEAFOOD" "MAISEL" "MASSED"
;; "MASSED" "MASOOD" "MASOOD" "MUSSEL" "MUSIL" "MOUSEL" "MOBSTER"
;; "MOBIL" "MOSIER")

(def test-cols
  [["S" "L" "SH" "SE" "M"]
   ["A" "U" "Q" "R" "O"]
   ["U" "F" "I" "B" "S"]
   ["M" "SE" "ST" "I" "OO"]
   ["ER" "P" "L" "D" "D"]])

(def test-cols-2
  [["A" "N" "K" "C" "B"]
   ["P" "A" "IT" "OO" "A"]
   ["R" "P" "CH" "K" "K"]
   ["O" "K" "E" "I" "I"]
   ["N" "IN" "N" "NG" "NG"]])

(def test-cols-3
  [["S" "M" "DE" "C" "L"]
   ["VE" "A" "E" "OF" "E"]
   ["T" "RN" "AD" "L" "LO"]
   ["PI" "ER" "WA" "EG" "L"]
   ["ON" "RE" "NG" "S" "IE"]])

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
