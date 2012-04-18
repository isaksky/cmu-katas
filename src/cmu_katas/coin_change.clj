(ns cmu-katas.coin-change)

;; You've just created a virtual vending machine that will dispense
;; widgets of programming goodness when a user puts money into the
;; machine. The machine should dispense the proper change. You now
;; need the programming logic to determine which coins to dispense.

;; Write a program that will correctly determine the least number of
;; coins to be given to the user such that the sum of the coins' value
;; would equal the correct amount of change.

;; Input: change amount
;; Input: coin denomination array (ie [1, 5, 10, 25, 100] )
;; Output: number of coins array (ie [1, 0, 1, 0, 0] would represent $0.11

;; For example
;; An input of 15 with [1, 5, 10, 25, 100] should return fifteen cents or [0, 1, 1, 0, 0]
;; An input of 40 with [1, 5, 10, 25, 100] should return two dimes or [0, 1, 1, 1, 0]

;; Part of the challenge with this exercise is to think through your
;; testing strategy. Do you test every possible input for the change
;; amount, or do you test specific boundary cases?

;; Use Test Driven Development (TDD) to solve this problem. Start
;; writing tests, write enough code to get the tests to pass, and then
;; refactor your code. Allow your design to emerge from writing tests;
;; don't try to solve the problem first.

;; If you prefer to return a hash instead of am array, that's ok too.
;; {1 => 1, 5 => 0, 10 => 1, 25 => 0, 100 => 0} equals $0.11

(def starting-coins [100 25 10 5 1])

(defn- change-priv [amt coins work]
  (if-not (seq coins) work
          (let [current-coin (first coins)
                num-fit (int (/ amt current-coin))]
            (change-priv (rem amt current-coin) (rest coins) (conj work num-fit)))))

(defn change [amt coins]
  (change-priv amt coins []))