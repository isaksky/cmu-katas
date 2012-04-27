;; A farmer has a forty pound stone that he uses to weigh food for his
;; animals on a scale. One day, he loans the stone to his neighbor. A
;; few days later, the neighbor returns the stone and apologizes to
;; the farmer because his stone is now broken in four pieces. The
;; farmer responds “Please don’t apologize, you have actually made my
;; life easier because with these four pieces, I can now measure any
;; weight between one and forty pounds”. The question is: how much do
;; these four individual pieces weigh?

(ns cmu-katas.stones
  (:require [clojure.math.combinatorics :as comb]))

(defn possible-stone-combos []
  (->> (comb/selections (range 1 41) 4)
       (filter (fn [c] (= 40 (reduce + c))))
       (map sort)
       distinct))

(defn- list-diff
  "What is in c1 that is not in c2?"
  [c1 c2]
  (filter (fn [e] (not-any? #{e} c2)) c1))

(defn can-weigh? [amt stone-combo]
  (->> (comb/subsets stone-combo)
       (some (fn [subset]
               (let [remain-subsets (comb/subsets (list-diff stone-combo subset))]
                 (->> remain-subsets
                      (some (fn [subset2] (= amt (- (reduce + subset)
                                         (reduce + subset2)))))))))))

(defn stones [possible-stone-combos]
  (->> possible-stone-combos
       (filter (fn [stone-combo]
                 (every? identity
                         (map (fn [amt] (can-weigh? amt stone-combo))
                              (range 1 41)))))))
