(ns aoc2022.day09
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

; Day 9: Rope Bridge

; common functions

(defn parse-input
  "Parses the given file into a seq of motions.
  Input: lines like \"X num\"
  Output: ((:x1 num1) (:x2 num2) ...)"
  [file-name]
  (->> (slurp file-name)
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[dir steps]] [(keyword (str/lower-case dir)) (util/parse-int steps)]))))

(defn plot-visited
  "Plots the :visited data as simple ascii on stdout, then returns `data`."
  [{:keys [visited] :as data}]
  (let [left (apply min (map first visited))
        right (apply max (map first visited))
        top (apply max (map second visited))
        bottom (apply min (map second visited))]
    ;(println "l" left "r" right "t" top "b" bottom)
    (doseq [y (range (+ top 2) (dec bottom) -1)
            x (range (dec left) (+ right 2))]
      (print (cond
              (= [0 0] [x y]) \s
              (visited [x y]) \#
              :else \.))
      (if (= x right) (println))))
  data)

(defn vec-add
  "Adds the two given vectors."
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn vec-sub
  "Subtracts the second vector from the first one."
  [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(def dir->vec
  {:u [0 1]
   :d [0 -1]
   :l [-1 0]
   :r [1 0]})

(defn adjecent?
  "Returns if the two points are not further away from each other
  than [1 1]."
  [[x0 y0] [x1 y1]]
  (and (<= (Math/abs (- x0 x1)) 1)
       (<= (Math/abs (- y0 y1)) 1)))

(defn shorten-1
  "Shortens a vector by 1. The longer side will be shortened. If
  both coordinates are equal then both get shortened.
  Example: [3 1] -> [2 1], [-5 -2] -> [-4 -2], [3 3] -> [2 2]"
  [[x y]]
  (cond
    (= (Math/abs x) (Math/abs y)) [((if (neg? x) inc dec) x) ((if (neg? y) inc dec) y)]
    (> (Math/abs x) (Math/abs y)) [((if (neg? x) inc dec) x) y]
    :else [x ((if (neg? y) inc dec) y)]))

(defn move-following
  "Moves `move-this` close to `follow-this` unless they already are
  adjecent. Returns the new value of `move-this` after moving (or the
  unchanged value if moving was not necessary.)"
  [move-this follow-this]
  ; only if moving is actually necessary
  (if (adjecent? move-this follow-this)
    move-this
    (vec-add move-this (shorten-1 (vec-sub follow-this move-this)))))

; part 1

(defn move-head
  "Move the :head of `data` according to the given `dir`-ection.
  Returns the updated `data`."
  [data dir]
  (update data :head vec-add (dir->vec dir)))

(defn exec-motion-simple
  "Executes `steps` into `dir`-ection for the two-knot rope. Expects `data`
  to have the two vecs :head and :tail as well as a :visited set.
  To be used in a reduce call. Returns the updated `data`."
  [data [dir steps]]
  (loop [i steps
         data data]
    (if (zero? i)
      data
      (recur (dec i)
             (let [data2 (move-head data dir)
                   tail-moved (move-following (data2 :tail) (data2 :head))]
               (-> data2
                   (assoc :tail tail-moved)
                   (update :visited conj tail-moved)))))))

(defn part-1
  "Positions visited by the tail of a 2-know long rope."
  []
  (as-> "res/input/day09.txt" $
    (parse-input $)
    (reduce exec-motion-simple {:head [0 0] :tail [0 0] :visited #{[0 0]}} $)
    (assoc $ :visited-count (count ($ :visited)))
    (dissoc $ :visited)
    #_(plot-visited $)))
; result: 6563

; part 2

(def rope-length 10)

(defn exec-motion-long
  "Executes `steps` into `dir`-ection for a `rope-length` long rope.
  Expects `data` to have a :knots vec and a :visited set.
  To be used in a reduce call. Returns the updated `data`."
  [data [dir steps]]
  (loop [i steps
         data data]
    (if (zero? i)
      data
      (recur (dec i)
             (let [data-head-moved (update data :knots update 0 vec-add (dir->vec dir))
                   data-all-moved (reduce (fn[data i]
                                            (update data :knots update i move-following (get-in data [:knots (dec i)])))
                                          data-head-moved
                                          (range 1 rope-length))
                   last-knot (get-in data-all-moved [:knots (dec rope-length)])]
               (update data-all-moved :visited conj last-knot))))))

(defn part-2
  "Positions visited by the tail of a 10-knot long rope."
  []
  (as-> "res/input/day09.txt" $
    (parse-input $)
    (reduce exec-motion-long {:knots (into [] (repeat rope-length [0 0])) :visited #{[0 0]}} $)
    (assoc $ :visited-count (count ($ :visited)))
    (dissoc $ :visited)
    #_(plot-visited $)))
; result: 2653
