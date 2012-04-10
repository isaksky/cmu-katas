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

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; Wait a second, the below assumes the weight of the stones have to
;; be different. Not necessarily true.

(def possible-stone-combos-1
  (filter (fn [c] (and (= 40 (reduce + c))))
          (comb/combinations (for [i (range 1 41)] i) 4)))

;; This one returns them all
(def possible-stone-combos-2
  (into #{}
        (filter (fn [c] (= 40 (reduce + c)))
                (map set (comb/selections (range 1 41) 4)))))

(defn- list-diff
  "What is in c1 that is not in c2?"
  [c1 c2]
  (filter (fn [e] (not-any? #{e} c2)) c1))

(defn can-weigh? [amt stone-combo]
  (some (fn [subset]
          (let [subset-sum (reduce + subset)
                remain-subsets (comb/subsets (list-diff stone-combo subset))]
            (some (fn [subset2] (= amt (- subset-sum
                                         (reduce + subset2))) )
                  remain-subsets)))
        (comb/subsets stone-combo)))

(defn stones [possible-stone-combos]
  (into #{}
        (filter (fn [stone-combo]
                  (every? identity
                          (map (fn [amt] (can-weigh? amt stone-combo))
                               (range 1 41))))
                possible-stone-combos)))
