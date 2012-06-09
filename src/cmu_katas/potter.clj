(ns cmu-katas.potter
  (:require [clojure.math.combinatorics :as c]))

;; One copy of any of the five books costs 8 EUR. If, however, you buy
;; two different books from the series, you get a 5% discount on those
;; two books. If you buy 3 different books, you get a 10% discount.
;; With 4 different books, you get a 20% discount. If you go the whole
;; hog, and buy all 5, you get a huge 25% discount.

;; Note that if you buy, say, four books, of which 3 are different
;; titles, you get a 10% discount on the 3 that form part of a set,
;; but the fourth book still costs 8 EUR.

;; Potter mania is sweeping the country and parents of teenagers
;; everywhere are queueing up with shopping baskets overflowing with
;; Potter books. Your mission is to write a piece of code to calculate
;; the price of any conceivable shopping basket, giving as big a
;; discount as possible.

(def base-book-price 8.00)

(defn pct-discount [num-unique-books]
  (case num-unique-books
    2 0.05
    3 0.10
    4 0.20
    5 0.25
    0))

(defn price
  "E.g., (price [1 2]) => 15.2"
  [bundle]
  (* (count bundle)
     (- base-book-price
        (* base-book-price
           (pct-discount (count (set bundle)))))))

(defn combos-price
  "E.g., (price [[1 2] [1 2]]) => 30.4"
  [combos]
  (reduce + (map price combos)))

(defn all-book-combinations [unique-potter-books]
  (reduce concat (map (fn [n] (c/combinations unique-potter-books n))
                      (range 2 6))))

(defn subtract-from-frequencies-map
  "E.g., (subtract-from-frequencies-map {1 1, 2 3} [1 2]) => {2 2}"
  [freq-map values]
  (->> (merge-with - freq-map (frequencies values))
       (remove (fn [[_ v]] (= 0 v)))
       (into {})))

(defn expand-frequencies-map
  "E.g., (expand-frequencies-map {1 1, 2 3}) => [1 2 2 2]"
  [freq-map]
  (mapcat (fn [[k v]] (repeat v k)) freq-map))

;; See tests for how to use
(def best-bundling
  (memoize
   (fn [book-freqs]
     (if-let [book-combinations (seq (all-book-combinations (keys book-freqs)))]
       (->> book-combinations
            (map (fn [book-combo] (concat [book-combo] (best-bundling (subtract-from-frequencies-map book-freqs book-combo)))))
            (sort-by #(combos-price %))
            first)
       [(expand-frequencies-map book-freqs)]))))
