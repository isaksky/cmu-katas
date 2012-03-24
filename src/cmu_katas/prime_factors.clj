(ns cmu-katas.prime-factors)

(defn find-first [pred coll]
  "Returns the first element the predicate is true for."
  (some (fn [e] (and (pred e) e))
        coll))

(def smallest-prime-factor
  (memoize
   (fn [n]
     (if (< n  2) (throw (Exception. "Too small")))
     (or (find-first (fn [i] (= 0 (rem n i)))
                     (for [n (range 2 (inc (.intValue (Math/sqrt n))))] n))
         n))))

(defn prime-factors [n]
  (let [spf (smallest-prime-factor n)
        quotient (/ n spf)]
    (if (= n spf) (list n)
        (cons spf (prime-factors quotient)))))

;; ----- DONE ----
;; Rest is just for fun

(defn prime-factors-iterative [n]
  (loop [i n
         memo (list)]
    (let [spf (smallest-prime-factor i)
          quotient (/ i spf)]
      (if (= i spf) (conj memo i)
          (recur quotient (conj memo spf))))))

(defn print-some-results []
  "Also doubles as test generator"
  (doseq [n (range 2 107)
          func-name ["prime-factors" "prime-factors-iterative"]]
    (println (str "(is (= '"
                  (sort (prime-factors n))
                  " (sort (" func-name " " n"))))"))))

(defn prime? [n]
  (= n (smallest-prime-factor n)))

(defn take-primes [n]
  (take n (filter prime? (drop 2 (range)))))