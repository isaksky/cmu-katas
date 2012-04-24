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

(def test-cols
  [["S" "L" "SH" "SE" "M"]
   ["A" "U" "Q" "R" "O"]
   ["U" "F" "I" "B" "S"]
   ["M" "SE" "ST" "I" "OO"]
   ["ER" "P" "L" "D" "D"]])

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