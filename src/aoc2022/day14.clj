(ns aoc2022.day14
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:require [aoc2022.util :as util]))

; Day 14: Regolith Reservoir

; common functions

(defn plot-map
  "Prints/plots the given cave to stdout.
  Input: {:map { y [:type
  Output:"
  [cave]
  (assert (map? cave))
  (doseq [y (range (:min-y cave) (inc (:max-y cave)))
          x (range (:min-x cave) (inc (:max-x cave)))]
    (let [c (get-in cave [:map y x] 0)
          p (or (:path cave) #{})]
      (print (case c
               :rock \#
               :sand \*
               :abyss \~
               (cond
                 (and (= x 500) (= y 0)) \+
                 (p [x y]) \~
                 (and (:ground cave) (= y (:max-y cave))) \#
                 :else \ )))
      (if (= x (:max-x cave))
        (println)))))

(defn parse-input
  "Parse given file and return a list of list of points.
  Input: Lines like: \"x1,y1 -> .. -> xN,yN\"
  Output: ( ( (x1 y1) .. (xN yN) )
            ... for every line ...)"
  [file-name]
  (->> (slurp file-name)
       str/split-lines
       (map #(re-seq #"\d+" %))
       (map #(map util/parse-int %))
       (map #(partition 2 %))))

(defn connect-two-points
  "Reduce fn for interpolating two points.
  Input: accumulator and two points
  Output: {y {x1 :rock x2 :rock .. xN :rock}} ; horizontal
      or  {y1 {x :rock} y2 {x :rock} .. yN {x :rock}} ; vertical"
  [map-data [[x1 y1] [x2 y2]]]
  (if (= y1 y2)
    (as-> (get map-data y1 {}) row
      (reduce #(assoc %1 %2 :rock) row (range (min x1 x2) (inc (max x1 x2))))
      (assoc map-data y1 row))
    (reduce #(update %1 %2 assoc x1 :rock) map-data (range (min y1 y2) (inc (max y1 y2))))))

(defn connect-points
  "Reduce fn for interpolating between points.
  Input: ((x1 y1) (x1 y2) (x2 y2) (x3 y2) ...)
         (only horizontal and vertical lines allowed)
  Output: Result of reducing pairs of points using `connect-two-points`."
  [map-data points]
  (->> points
       (partition 2 1)
       (reduce connect-two-points map-data)))

(defn update-size
  "Analyzes the cave's map-data and returns `cave` with the size added
  (i.e. assoc'ed) as keys :min-x, :max-x, :min-y and :max-y."
  [cave]
  (let [map-data (:map cave)
        all-x (flatten (map keys (vals map-data)))]
    (assoc cave
     :min-x (apply min all-x)
     :max-x (apply max all-x)
     :min-y 0 #_(apply min (filter number? (keys map-data)))
     :max-y (apply max (filter number? (keys map-data))))))

(defn input->cave
  "Build a cave data structure out of the input.
  Input: result of `parse-input`
  Output: {:map {y1 {x1 :rock .. xN :rock} .. yM {..}}
           :min-x <n> :max-x <n>
           :min-y <n> :max-y <n> }"
  [data]
  (->> data
       (reduce connect-points {}) ; raw data becomes map-data
       (assoc {} :map) ; create `cave` data structure with map-data as :map
       update-size)) ; add sizes according to map-data

(defn drop-sand
  "Drops one piece of sand as long as possible.
  It stops when the sand comes to rest or reaches the abyss.
  It returns the cave with the sand in its terminal location,
  marked as either :sand or :abyss.
  The returned cave also contains a key :path whose value is 
  a vector with the path the sand took."
  [cave]
  (loop [x 500 y 0 ; start at 500,0
         t 10000 ; hard limit in case of infinite loops ;)
         p #{}] ; track the path of the current sand
    (if (zero? t)
      (throw (ex-info "too many iterations in the sand" {:x x :y y})))
    (let [y' (inc y)]
      (cond
        ; has sand fallen to far?
        ; (part-2 with ground: sand comes to rest)
        (and (:ground cave)
             (= y (dec (:max-y cave)))) (-> cave 
                                            (assoc-in [:map y x] :sand)
                                            (assoc :path p))
        ; (part-1 with abyss: add abyss markers)
        (> y (:max-y cave)) (-> cave 
                                (assoc-in [:map y x] :abyss)
                                (assoc :abyss [x y])
                                (assoc :path p))
        ; can sand move straight down?
        (nil? (get-in cave [:map y' x])) (recur x y' (dec t) (conj p [x y']))
        ; can sand move down and left?
        (nil? (get-in cave [:map y' (dec x)])) (recur (dec x) y' (dec t) (conj p [(dec x) y']))
        ; can sand move down and right?
        (nil? (get-in cave [:map y' (inc x)])) (recur (inc x) y' (dec t) (conj p [(inc x) y']))
        ; otherwise sand comes to rest.
        :else (-> cave 
                  (assoc-in [:map y x] :sand)
                  (assoc :path p))))))

(defn plot-and-return
  "Plots the last cave (with one more `drop-sand` added and updated size),
  then returns the original `caves` collection."
  [caves]
  (-> caves
      ; take last and drop-sand one more time
      ; so we also have the :abyss in the plot
      last
      drop-sand
      ; adjust plot size
      update-size
      ((fn[cave] (if (:ground cave) 
                   (-> cave
                     (update :min-x - 2)
                     (update :max-x + 2)
                     (update :max-y inc))
                   cave)))
      ; plot!
      plot-map)
  caves)


; part 1

(defn part-1
  "Sand falls and rests, until abyss."
  []
  (->> (parse-input "res/input/day14.txt")
       input->cave
       (iterate drop-sand)
       ; iterate until sand reaches abyss
       (take-while (complement :abyss))
       ;plot-and-return
       count
       dec)) ; first "iteration" is original input
; result: 779 

  
; part 2

(defn part-2
  "Sand falls and rests, until origin (500,0) is blocked."
  []
  (->> (parse-input "res/input/day14.txt")
       input->cave
       ; add ground floor
       ((fn[cave] (-> cave (update :max-y + 2) (assoc :ground true))))
       (iterate drop-sand)
       ; iterate while 0/500 is not blocked by sand
       (take-while (fn[cave] (not= :sand (get-in cave [:map 0 500]))))
       ;plot-and-return
       count))
; result: 27426
