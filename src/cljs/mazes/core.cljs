(ns ^:figwheel-always mazes.core
    (:require [clojure.string :as str]))

(enable-console-print!)

;; grid

(defn make-grid [rows columns]
  {:rows rows :columns columns :links {}})

(defn link-cells [grid cell-a cell-b]
  (assert (not (nil? cell-a)))
  (assert (not (nil? cell-b)))
  (-> grid
      (update-in [:links cell-a] #(conj (or % #{}) cell-b))
      (update-in [:links cell-b] #(conj (or % #{}) cell-a))))

(defn linked-to? [grid cell-a cell-b]
  (contains? (get-in grid [:links cell-a] #{}) cell-b))

(defn valid-pos? [{:keys [rows columns]} [y x]]
  (and (>= x 0) (< x columns)
       (>= y 0) (< y rows)))

(defn cell [grid [y x :as cell]]
  (if (valid-pos? grid cell)
    {:x x :y y :links (get-in grid [:links cell] #{})}))

(defn rand-cell [{:keys [rows columns]}] [(rand-int rows) (rand-int columns)])

(defn cells-seq [{:keys [rows columns]}]
  (for [y (range rows) x (range columns)]
    [y x]))

(defn rows-seq [{:keys [rows columns]}]
  (for [y (range rows)]
    (for [x (range columns)]
      [y x])))

(defn east  [[y x]] [y (inc x)])
(defn west  [[y x]] [y (dec x)])
(defn north [[y x]] [(dec y) x])
(defn south [[y x]] [(inc y) x])

(defn cell-neighbors [cell]
  [(north cell) (east cell) (south cell) (west cell)])

(defn accessible-neighbors [grid cell]
  (filter (partial linked-to? grid cell) (cell-neighbors cell)))

;; maze generators

(defn binary-tree-link-cell [{:keys [rows columns] :as grid} [y x :as cell] direction]
  (cond
    (not (valid-pos? grid cell))      nil
    (and (= y 0) (= x (dec columns))) nil
    (= y 0)              (east cell)
    (= x (dec rows))     (north cell)
    (= :east direction)  (east cell)
    (= :north direction) (north cell)))

(defn gen-binary-tree [grid]
  (reduce (fn [grid cell]
            (if-let [link-cell (binary-tree-link-cell grid cell (rand-nth [:north :east]))]
              (link-cells grid cell link-cell)
              grid))
          grid (cells-seq grid)))

(defn gen-sidewinder [grid]
  (reduce (fn [grid row]
            (let [past (atom [])]
              (reduce (fn [grid cell]
                        (swap! past conj cell)
                        (let [eastern-boundary? (not (valid-pos? grid (east cell)))
                              norther-boundary? (not (valid-pos? grid (north cell)))
                              close-out? (or eastern-boundary? (and (not norther-boundary?)
                                                                    (= 0 (rand-int 2))))]
                          (if close-out?
                            (let [member (rand-nth @past)]
                              (reset! past [])
                              (if (valid-pos? grid (north member))
                                (link-cells grid member (north member))
                                grid))
                            (link-cells grid cell (east cell)))))
                      grid row)))
          grid (rows-seq grid)))

;; solvers

(defn dijkstra-enumerate [grid start-cell]
  (loop [marks {}
         queue [[start-cell 0]]]
    (if (seq queue)
      (let [set-marks (set (keys marks))
            [cell distance] (first queue)
            neighbors (->> (accessible-neighbors grid cell)
                           (remove (partial contains? set-marks))
                           (map #(vector % (inc distance))))]
        (recur (assoc marks cell distance)
               (concat (rest queue) neighbors)))
      marks)))

(defn trace-route-back [grid marks start-cell]
  (loop [path [start-cell]
         cell start-cell]
    (let [next (->> (accessible-neighbors grid cell)
                    (reduce #(if (< (get marks %1) (get marks %2)) %1 %2)))]
      (if (> (get marks next) 0)
        (recur (conj path next) next)
        (conj path next)))))

(defn farthest-point [marks]
  (->> (reduce (fn [[_ max :as current] [_ distance :as next]]
                 (if (> distance max) next current))
               [nil 0] marks)
       first))

(defn longest-path-enum [grid]
  (->> (dijkstra-enumerate grid [0 0])
       (farthest-point)
       (dijkstra-enumerate grid)))

(defn longest-path [grid]
  (let [longest-enum (longest-path-enum grid)]
    (trace-route-back grid longest-enum (farthest-point longest-enum))))

;; output

(defn ascii-grid
  ([grid] (ascii-grid grid (fn [_ _] "   ")))
  ([{:keys [columns rows] :as grid} content-maker]
   (let [str-repeat #(str/join "" (repeat %1 %2))
         header (str "+" (str-repeat columns "---+") "\n")
         lines (for [row (range rows)
                     :let [parts (for [column (range columns)
                                       :let [cell [row column]]]
                                   [(str " " (content-maker grid cell) " " (if (linked-to? grid cell (east cell)) " " "|"))
                                    (str (if (linked-to? grid cell (south cell)) "   " "---") "+")])]]
                 (str "|" (str/join "" (map first parts)) "\n"
                      "+" (str/join "" (map second parts)) "\n"))]
     (apply str header lines))))

(defn canvas-line [ctx [x1 y1] [x2 y2]]
  (doto ctx
    (.beginPath)
    (.moveTo x1 y1) (.lineTo x2 y2)
    (.stroke)))

(defn cell-bounds [[y x] cell-size]
  [(* x cell-size) (* y cell-size)
   (* (inc x) cell-size) (* (inc y) cell-size)])

(defn draw-grid [ctx grid cell-size]
  (doseq [cell (cells-seq grid) :let [[x1 y1 x2 y2] (cell-bounds cell cell-size)]]
    (if-not (valid-pos? grid (north cell)) (canvas-line ctx [x1 y1] [x2 y1]))
    (if-not (valid-pos? grid (west cell)) (canvas-line ctx [x1 y1] [x1 y2]))
    (if-not (linked-to? grid cell (east cell)) (canvas-line ctx [x2 y1] [x2 y2]))
    (if-not (linked-to? grid cell (south cell)) (canvas-line ctx [x1 y2] [x2 y2]))))

(defn draw-backgrounds [ctx marks cell-size]
  (let [max-distance (get marks (farthest-point marks))
        color-at (fn [x] (->> (/ x max-distance)
                              (* 100)
                              (.round js/Math)
                              (- 255)))]
    (doseq [[cell distance] marks
            :let [color (color-at distance)
                  [x y] (cell-bounds cell cell-size)]]
      (doto ctx
        (aset "fillStyle" (str "rgb(" color ", " 255 ", " color ")"))
        (.fillRect x y cell-size cell-size)))))

;; misc

(defn sample-canvas-draw []
  (let [canvas (.querySelector js/document "#sample-canvas")
        ctx (.getContext canvas "2d")
        grid (-> (make-grid 15 15) gen-sidewinder)
        cell-size 30]
    (doto ctx
      (.clearRect 0 0 (.-width canvas) (.-height canvas))
      (.save)
      (.translate 10 10)
      (draw-backgrounds (longest-path-enum grid) cell-size)
      (draw-grid grid cell-size)
      (.restore))))
