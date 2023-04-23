(ns aoc2022.day12
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util])
  (:require [aoc2022.day12plot :as day12plot])
  (:require [clojure.data.priority-map :refer [priority-map]])
  (:import [javax.swing JFrame])
  (:import [java.awt Color]))

; Day 12: Hill Climbing Algorithm

; common functions

(defn parse-input
  "Parses the given input and return a map like
  {:map <nested vector, the outer being rows, the inner cols>
   :start-rc [start-row start-column] 
   :end-rc [end-row end-column]"
  [input]
  (let [map-data (vec (map vec (str/split-lines input)))
        start-rc (vec (ffirst (keep-indexed (fn [r row] (seq (keep-indexed (fn[c v] (if (= v \S) [r c])) row))) map-data)))
        end-rc (vec (ffirst (keep-indexed (fn [r row] (seq (keep-indexed (fn[c v] (if (= v \E) [r c])) row))) map-data)))]
    {:map map-data
     :start-rc start-rc
     :end-rc end-rc}))

(defn get-height
  "Returns the height at the given row/col.
  Heights are \\a = 1 thru \\z = 26, and the special
  values \\S =1 and \\E = 26."
  [data r c]
  (let [h (get-in (:map data) [r c] nil)]
    (case h
      nil nil
      \S 1
      \E 26
      (- (int h) 96)))) ; \a - 96 = 1

; dijkstra algorithm

(defn init-dij-data
  "Adds to the given data map key/values for the dijkstra algorithm."
  [data start-rc]
  (assoc data
         :abs-costs {start-rc 0}  ; absolute costs so far from start to [r c]
         :parents {start-rc start-rc} ; best parent [p_r p_c] so far for [r c], init with {start-rc start-rc}
         :queue (priority-map start-rc 0))) ; priority map, i.e. map sorted by values (not keys)

(defn dij-get-costs
  "Returns the costs between from and to.
  Costs are simply ints where higher values mean higher costs."
  [data [from-r from-c] [to-r to-c]]
  (let [from-h (get-height data from-r from-c)
        to-h (get-height data to-r to-c)
        diff (- to-h from-h)]
    ; climbing up is what we want in the end
    ; so encourage paths that go upward by returning
    ; lower costs for them
    (case diff
      ; upwards -> cost 1
      1 2
      ; equal height -> as good as climbing -> cost 1
      0 1
      ; downwards -> costs more
      (- 3 diff))))

(defn dij-path
  "Returns the from :start-rc to the given end.
  Return value is a vector of [row col] pairs."
  [parents-map [end-r end-c]]
  (loop [path (list [end-r end-c]) 
         visited #{}]
    (let [curr (first path)
          parent (parents-map curr)]
      (if (visited curr)
        (println "already visited:" curr))
      (if (or (nil? parent) (= parent curr) (visited curr))
        path
        (recur (conj path parent)
               (conj visited curr))))))

(defn dij-get-neighbors
  "Returns a seq with the neighbors of the given row/col."
  [data [r c]]
  (let [next-height (inc (get-height data r c))]
    (for [[r' c'] (list [r (dec c)] [(dec r) c] [r (inc c)] [(inc r) c])
          :when (and (get-height data r' c')
                     (<= (get-height data r' c') next-height))]
      [r' c'])))

(defn dijkstra-reducer
  "Dijkstra logic (part 2).
  Input:
  - curr: current node
  - data: algorithm data so far, see `run-dijkstra`
  - nbr: neighbor of `curr` to check
  Output:
  - Map like `data`"
  [curr data nbr]  ; nbr: neighbor (of curr) to check
  (let [abs-costs ((:abs-costs data) curr)
        new-costs (+ abs-costs (dij-get-costs data curr nbr))] ; todo replace dij-get-costs by (:cost-fn data)
    ; if in queue but new-costs is better (lower),
    ; or not visited yet (no parent)?
    (if (or (and (contains? (:queue data) nbr)
                 (< new-costs abs-costs))
            (not (contains? (:parents data) nbr)))
      ; then enqueue (or update queue priority) and
      ; update value and parent
      (-> data
          (update :abs-costs assoc nbr new-costs)
          (update :parents assoc nbr curr)
          (update :queue assoc nbr new-costs))
      ; else no changes
      data)))

;;; see day12plot.clj below for console and JFrame plotting
(defn plot-any
  [plot-ctx data]
  nil)

(defn run-dijkstra
  "Dijkstra logic (part 1).
  Input:
  - data: algorithm data as map:
    {:abs-costs {[r c] val, ...}  ; absolute costs so far from start to [r c]
     :parents {[r c] [p_r, p_c], ...} ; best parent [p_r p_c] so far for [r c]
     :queue {[r c] val, ...}} ; priority map, i.e. map sorted by values (not keys)
  Initially the queue should contain the start node for the algorithm to start."
  ([data] (run-dijkstra data nil))
  ([data plot-ctx]
   (let [curr (first (peek (:queue data)))
         neigh (dij-get-neighbors data curr) ; replace dij-get-neighbors by (:neighbors-fn)
         data-next (reduce (partial dijkstra-reducer curr)
                           (assoc data :queue (pop (:queue data)))
                           neigh)
         plot-ctx (plot-any plot-ctx (assoc data-next :end-rc curr))]
     (if (empty? (:queue data-next))
       data-next
       (recur data-next plot-ctx)))))

(defn calc-dij-distance
  "Returns the number of steps from :start-rc to :end-rc."
  [dij-result]
  (let [path (dij-path (:parents dij-result) (:end-rc dij-result))]
    {:count (dec (count path))}))

; part 1

; to activate console plotting
; 1. uncomment the following:
;(def plot-any day12plot/plot-console)
; 2. add aoc2022.day12/part-1 to core.-main

(defn part-1
  "Shortest path from S to E."
  []
  (plot-any nil nil)
  (let [map-data (parse-input (util/get-input 12))
        map-data (assoc map-data
                        :get-height get-height
                        :dij-path dij-path)
        dij-data (init-dij-data map-data (:start-rc map-data))
        dij-result (run-dijkstra dij-data)]
    (plot-any nil dij-result)
    (println "steps:" (:count (calc-dij-distance dij-result)))))
; result: 447

; part 2

(defn part-2
  "Shortest path from any a to E."
  []
  ; loop over all a's, execute run-dijkstra and calc-dij-distance 
  ; and return the lowest distance value
  (let [map-data (parse-input (util/get-input 12))
        dij-data (init-dij-data map-data (:start-rc map-data))
        width (count (:map dij-data))
        height (count (get (:map dij-data) 0))
        plot-ctx (atom nil)
        a-distances (for [r (range height)
                          c (range width)
                          :when (#{\a \S} (get-in (:map dij-data) [r c]))]
                      (do
                        (print "row col:" r c "...   \r") (flush)
                        (let [res (-> dij-data
                                      (assoc :start-rc [r c])
                                      (init-dij-data [r c])
                                      run-dijkstra)]
                          (reset! plot-ctx (plot-any @plot-ctx res))
                          [[r c] (calc-dij-distance res)])))
        best-path (->> a-distances 
                       (remove #(= 0 (:count (second %))))
                       (apply min-key (comp :count second)))]
    (plot-any nil (-> dij-data
                           (assoc :start-rc (first best-path))
                           (init-dij-data (:start-rc dij-data))
                           run-dijkstra))
    (println "best path from" (first best-path) "with" (second best-path) "steps")))
; result: 446 starting at [19 0]
