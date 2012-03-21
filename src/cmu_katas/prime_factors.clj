(ns cmu-katas.prime-factors)

(defn smallest-prime-factor [n]
  (if (< n  2) (throw (Exception. "Too small")))
  (some (fn [i] (and (= 0 (rem n i))
                    i))
        (for [n (range 2 (inc n))] n)))

(defn prime-factors [n]
  (let [spf (smallest-prime-factor n)
        quotient (/ n spf)]
    (if (= n spf) [n]
        (conj (flatten (prime-factors quotient)) spf))))

;; ----- DONE ----
;; Rest is just for fun

(defn prime-factors-iterative [n]
  (loop [i n
         memo []]
    (let [spf (smallest-prime-factor i)
          quotient (/ i spf)]
      (if (= i spf) (conj memo i)
          (recur quotient (conj memo spf))))))

(defn print-some-results []
  (doseq [n (range 2 107)]
    (println (str n ": " (prime-factors n)))))

;; Results
;; 2: [2]
;; 3: [3]
;; 4: (2 2)
;; 5: [5]
;; 6: (2 3)
;; 7: [7]
;; 8: (2 2 2)
;; 9: (3 3)
;; 10: (2 5)
;; 11: [11]
;; 12: (2 2 3)
;; 13: [13]
;; 14: (2 7)
;; 15: (3 5)
;; 16: (2 2 2 2)
;; 17: [17]
;; 18: (2 3 3)
;; 19: [19]
;; 20: (2 2 5)
;; 21: (3 7)
;; 22: (2 11)
;; 23: [23]
;; 24: (2 2 2 3)
;; 25: (5 5)
;; 26: (2 13)
;; 27: (3 3 3)
;; 28: (2 2 7)
;; 29: [29]
;; 30: (2 3 5)
;; 31: [31]
;; 32: (2 2 2 2 2)