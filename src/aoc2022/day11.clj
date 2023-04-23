(ns aoc2022.day11
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

; Day 11: Monkey in the Middle

; common functions

(defn parse-monkey
  "Output: {:num n
           :items [n n ...]
           :worry-fn op :worry-arg n 
           :rem-test-arg n
           :rem-test-true n :rem-test-false n}"
  [lines]
  (reduce (fn foo[data line]
            (cond 
              (str/includes? line "Monkey")
              (assoc data
                     :num (util/parse-int (first (re-seq #"\d+" line))))
              (str/includes? line "Starting")
              (assoc data
                     :items (mapv util/parse-int (re-seq #"\d+" line)))
              (str/includes? line "Operation:")
              (assoc data
                     :worry-fn (if (str/includes? line "+") +' *')
                     :worry-arg (util/parse-int (first (re-seq #"\d+" line))))
              (str/includes? line "Test")
              (assoc data
                     :rem-test-arg (util/parse-int (first (re-seq #"\d+" line))))
              (str/includes? line "If true")
              (assoc data
                     :rem-test-true (util/parse-int (first (re-seq #"\d+" line))))
              (str/includes? line "If false")
              (assoc data
                     :rem-test-false (util/parse-int (first (re-seq #"\d+" line))))
              :else data))
          {:inspect-count 0}
          lines))

(defn parse-input
  "Parses the given input into a vec of monkey data (see `parse-monkey`)."
  [input]
  (->> input
       str/split-lines
       (partition-by #(= "" %))
       (take-nth 2)
       (mapv parse-monkey)))

; part 1

(defn calc-worry-with-relief
  "Calc worry level with relief (div by 3)."
  [monkey item]
  (-> item
      ((monkey :worry-fn) (or (monkey :worry-arg) item))
      (/ 3)
      int))

(defn play-turn
  "Play the turn of monkey with the given number `mk-nr`.
  The `calc-worry-fn` receives the current monkey and the item and shall
  return the item with the new worry level."
  ; for each item, apply op, divide by 3, apply test, move to next monkey
  [calc-worry-fn monkeys mk-nr]
  (let [curr-monkey (monkeys mk-nr)
        ; current monkeys items
        curr-items (curr-monkey :items)
        ; increment inspect-count
        monkeys (update monkeys mk-nr update :inspect-count + (count curr-items))
        ; empty current monkeys item list
        monkeys (assoc-in monkeys [mk-nr :items] [])]
    ; for each in current items: calc & test the worry level and move the item
    (reduce (fn[monkeys item]
              (let [worry-item (calc-worry-fn curr-monkey item)
                    next-monkey (if (zero? (rem worry-item (curr-monkey :rem-test-arg)))
                                  (curr-monkey :rem-test-true)
                                  (curr-monkey :rem-test-false))]
                (update monkeys next-monkey update :items conj worry-item)))
            monkeys
            curr-items)))

(defn play-round
  "Play one round with all monkeys."
  [calc-worry-fn monkeys]
  (reduce (partial play-turn calc-worry-fn) monkeys (range (count monkeys))))

(defn part-1
  "Follow Monkeys for 20 rounds."
  []
  (let [monkeys (parse-input (util/get-input 11))]
    (->> monkeys
         (iterate (partial play-round calc-worry-with-relief))
         (#(nth % 20))
         (map :inspect-count)
         (sort >)
         (take 2)
         (apply *))))
; result: 99840

; part 2

(defn calc-worry-without-relief
  "Calc worry level withOUT relief (no div by 3)."
  [worry-limit monkey item]
  (-> item
      ((monkey :worry-fn) (or (monkey :worry-arg) item))
      ; don't let numbers grow to big:
      (rem worry-limit)))

(defn part-2
  "Monkeys play 10,000 rounds..."
  []
  (let [monkeys (parse-input (util/get-input 11))
        ; to decide what monkey to throw an item to the monkeys test the 'worry' items.
        ; these tests are always divisions by various numbers. our worry levels don't
        ; need to be much bigger numbers than these divisors for the test to work.
        ; in particular if we calculate the product of all divisors of all monkeys
        ; then we have the highest relevant meta-divisor for all monkey tests to work.
        ; values bigger than that don't contribute any more to the tests.
        ; this trick helps us keep the item numbers from skyrocketing.
        worry-limit (->> monkeys (map :rem-test-arg) (apply *))]
    (->> monkeys
         (iterate (partial play-round (partial calc-worry-without-relief worry-limit)))
         (#(nth % 10000))
         (map :inspect-count)
         (sort >)
         (take 2)
         (apply *))))
; result: 20683044837
