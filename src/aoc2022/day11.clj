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
  "Parses the given file into a vec of monkey data (see `parse-monkey`)."
  [file-name]
  (->> (slurp file-name)
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
  "Play the turn of monkey with the given number `mk-nr`."
  ; for each item, apply op, divide by 3, apply test, move to next monkey
  [calc-worry-fn worry-limit monkeys mk-nr]
  (let [curr-monkey (monkeys mk-nr)
        ; current monkeys items
        curr-items (curr-monkey :items)
        ; increment inspect-count
        monkeys (update monkeys mk-nr update :inspect-count + (count curr-items))
        ; empty current monkeys item list
        monkeys (assoc-in monkeys [mk-nr :items] [])]
    ; for each in current items: calc & test the worry level and move the item
    (reduce (fn[monkeys item]
              (let [worry-level (calc-worry-fn curr-monkey item)
                    worry-level (if worry-limit (rem worry-level worry-limit) worry-level)
                    next-monkey (if (zero? (rem worry-level (curr-monkey :rem-test-arg)))
                                  (curr-monkey :rem-test-true)
                                  (curr-monkey :rem-test-false))]
                (update monkeys next-monkey update :items conj worry-level)))
            monkeys
            curr-items)))

(defn play-round
  "Play one round with all monkeys."
  [calc-worry-fn worry-limit monkeys]
  (reduce (partial play-turn calc-worry-fn worry-limit) monkeys (range (count monkeys))))

(defn part-1
  "Follow Monkeys for 20 rounds."
  []
  (let [monkeys (parse-input "res/input/day11.txt")]
    (->> monkeys
         (iterate (partial play-round calc-worry-with-relief nil))
         (#(nth % 20))
         (map :inspect-count)
         (sort >)
         (take 2)
         (apply *))))
; result: 99840

; part 2

(defn calc-worry-without-relief
  "Calc worry level withOUT relief (no div by 3)."
  [monkey item]
  (-> item
      ((monkey :worry-fn) (or (monkey :worry-arg) item))))

(defn part-2
  "Monkeys play 10,000 rounds..."
  []
  (let [monkeys (parse-input "res/input/day11.txt")
        worry-limit (->> monkeys (map :rem-test-arg) (apply *))]
    (->> monkeys
         (iterate (partial play-round calc-worry-without-relief worry-limit))
         (#(nth % 10000))
         (map :inspect-count)
         (sort >)
         (take 2)
         (apply *))))
; result: 20683044837
