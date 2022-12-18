(ns aoc2022.day12plot
  (:import [javax.swing JFrame])
  (:import [java.awt Color]))

; Day 12: Hill Climbing Algorithm - plotting functions

;;; plotting to console

; more color codes see: https://i.stack.imgur.com/KTSQa.png
(def color-codes {:red 9 :green 10})

(defn esc
  "Construct an ANSI escape sequence with given args."
  [& args]
  (reduce (fn[res one-args]
            (str res "\033[" (apply str one-args)))
          ""
          args))

(defn ansi-color
  [string color]
  (str (esc ["38;5;" color "m"]) string (esc "m")))

(defn get-console-terrain-col
  [data [r c]]
  (int (+ 235 (* 20 (/ ((:get-height data) data r c) 27.0)))))

(defn plot-console
  [plot-ctx data]
  (if (nil? data)
    ; no data -> clear screen etc
    (do
      ; erase display, place cursor in top left corner, hide cursor
      (print (esc "2J" "H" "?25l"))
      (flush))
    ; else plot data
    (let [height (count (:map data))
          width-1 (dec (count (get (:map data) 0)))
          path ((:dij-path data) (:parents data) (:end-rc data))]
      (println (esc "H") "start:" (:start-rc data) "end:" (:end-rc data) (if (nil? plot-ctx) "(no ctx)" "(with ctx)") "   ")
      (if (nil? plot-ctx)
        ; draw complete field
        (do
          (doseq [r (range height)
                  c (range (inc width-1))]
            (print (ansi-color 
                     ; char to plot:
                     (get-in (:map data) [r c])
                     ; color to use:
                     (get-console-terrain-col data [r c])))
            ; add line break after each row
            (if (= c width-1)
              (println)))
          ; save the cursor location if we plot the whole thing
          (print (esc "s"))
          (flush))
        ; only re-draw previous path
        (doseq [rc (remove (set path) (:previous-path plot-ctx))]
          (print (str (esc [(+ 2 (first rc)) ";" (inc (second rc)) "H"])
                      (ansi-color 
                        ; char to plot:
                        (get-in (:map data) rc)
                        ; color to use:
                        (get-console-terrain-col data rc))))))
      ; draw new path
      (doseq [rc path]
        (print (str (esc [(+ 2 (first rc)) ";" (inc (second rc)) "H"])
                    (ansi-color
                      ; char to plot:
                      (get-in (:map data) rc)
                      ; color to use:
                      (if (or (= rc (:start-rc data))
                              (= rc (:end-rc data)))
                        (color-codes :green)
                        (color-codes :red))))))
      ; restore cursor location if we plotted the whole thing
      (if (nil? plot-ctx)
        (println (esc "u")))
      {:previous-path path})))

;;; plotting to console

(def terrain-colors
  (->> (range (int \a) (inc (int \z)))
       (map (fn[i] (let [gray (-> i (- 96) (/ 26) (* 0.75) (+ 0.25) float)] [(char i) (Color. gray gray gray)])))
       (into {})))

(def visited-colors
  (->> (range (int \a) (inc (int \z)))
       (map (fn[i] (let [gray (-> i (- 96) (/ 26) (* 0.75) (+ 0.25) float)
                         lighter (+ (* gray 0.8) 0.2)] [(char i) (Color. gray gray lighter)])))
       (into {})))

; use: 
; (def frame (init-frame))
; (def plot-any (partial #'plot-frame frame))))

(defn init-frame
  []
  (let [f (JFrame. "AoC 2022, Day 12: Hill Climbing")]
    (.setDefaultCloseOperation f JFrame/DISPOSE_ON_CLOSE)
    (.setSize f 1500 640)
    (.setVisible f true)
    (.setLocationRelativeTo f nil)
    (.setBackground (.getContentPane f) Color/black)
    f))

(defn plot-frame
  [frame plot-ctx data]
  (let [g (.getGraphics frame)
        height (count (:map data))
        width (count (get (:map data) 0))
        path ((:dij-path data) (:parents data) (:end-rc data))
        scale-r 11
        scale-c 9
        rect-l 7
        rect-l2 (int (/ rect-l 2))
        off-r 48
        off-c 16]
    (.setBackground g Color/black)
    (.clearRect g 0 0 200 30)
    (.setColor g Color/white)
    (.drawString g (str "Start: " (:start-rc data) ", end: " (:end-rc data)) 20 20)
    (if (nil? plot-ctx)
      ; draw complete field
      (doseq [r (range height)
              c (range width)]
        (let [xr (+ off-r (* scale-r r))
              xc (+ off-c (* scale-c c))
              height (get-in (:map data) [r c])]
          (.setColor g (terrain-colors height))
          (.fillRect g (- xc rect-l2) (- xr rect-l2) rect-l rect-l)))
      ; only re-draw previous path
      (doseq [[r c] (remove (set path) (:previous-path plot-ctx))]
        (let [xr (+ off-r (* scale-r r))
              xc (+ off-c (* scale-c c))
              height (get-in (:map data) [r c])]
          (.setColor g (visited-colors height))
          (.fillRect g (- xc rect-l2) (- xr rect-l2) rect-l rect-l))))
    ; draw new path
    (doseq [[r c] path]
      (let [xr (+ off-r (* scale-r r))
            xc (+ off-c (* scale-c c))
            height (get-in (:map data) [r c])]
        (.setColor g (cond
                       ; start/end -> green
                       (or (= [r c] (:start-rc data)) (= [r c] (:end-rc data))) Color/green
                       ; path -> red
                       :else Color/red))
        (.fillRect g (- xc rect-l2) (- xr rect-l2) rect-l rect-l)))
    {:previous-path path}))
