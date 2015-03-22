(ns ^:figwheel-always mazes.core
  (:require-macros [wilkerdev.util.macros :refer [bench]])
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(enable-console-print!)

(defprotocol IHaveCells
  (cells-seq [grid])
  (valid-pos? [grid cell])
  (count-cells [grid])
  (rand-cell [grid])
  (cell-neighbors [grid cell]))

(defprotocol IGridSerialize
  (grid-type-key [this]))

(defn serialize-grid [grid]
  (-> (into {} grid)
      (assoc :grid-type (grid-type-key grid))))

(defmulti unserialize-grid* :grid-type)

(defn unserialize-grid [grid]
  (-> (unserialize-grid* grid)
      (dissoc :grid-type)))

;; util

(defn index-of [value coll]
  (first (keep-indexed #(if (= %2 value) %1) coll)))

;; rectangular grid

(defn east  [[y x]] [y (inc x)])
(defn west  [[y x]] [y (dec x)])
(defn north [[y x]] [(dec y) x])
(defn south [[y x]] [(inc y) x])

(defrecord RectangularGrid [rows columns links mask]
  IHaveCells
  (cells-seq [this]
    (for [y (range rows) x (range columns)
          :let [cell [y x]]
          :when (valid-pos? this cell)]
      cell))

  (valid-pos? [_ [y x :as cell]]
    (and (>= x 0) (< x columns)
         (>= y 0) (< y rows)
         (not (contains? mask cell))))

  (count-cells [this]
    (-> (* rows columns) (- (count (filter (partial valid-pos? (assoc this :mask #{})) mask)))))

  (rand-cell [_]
    (loop []
      (let [cell [(rand-int rows) (rand-int columns)]]
        (if (contains? mask cell) (recur) cell))))

  (cell-neighbors [_ cell] #{(north cell) (east cell) (south cell) (west cell)})

  IGridSerialize
  (grid-type-key [_] ::rectangular))

(defn make-grid
  ([attrs] (merge (make-grid 0 0) attrs))
  ([rows columns]
   (RectangularGrid. rows columns {} #{})))

(defmethod unserialize-grid* ::rectangular [attrs] (make-grid attrs))

;; polar grid

(defn make-polar-row [row count] (map #(vector row %) (range count)))

(def polar-count-row-cells
  (memoize (fn polar-count-row-cells [rows y]
             {:pre (>= y 0)}
             (if (= y 0)
               1
               (let [radius (/ y rows)
                     circumference (* 2 Math/PI radius)
                     previous (polar-count-row-cells rows (dec y))
                     estimated (/ circumference previous)
                     ratio (/ estimated (/ 1 rows))]
                 (* previous (.round js/Math ratio)))))))

(defn polar-cell-cw [{:keys [rows]} [y x :as cell]]
  (if (= cell [0 0])
    [0 0]
    [y (mod (inc x) (polar-count-row-cells rows y))]))

(defn polar-cell-ccw [{:keys [rows]} [y x :as cell]]
  (if (= cell [0 0])
    [0 0]
    [y (mod (dec x) (polar-count-row-cells rows y))]))

(defn polar-cell-inward [{:keys [rows]} [y x]]
  (if (= y 0)
    [(dec y) x]
    (let [prev-count (polar-count-row-cells rows (dec y))
          ratio (min 2 (/ (polar-count-row-cells rows y) prev-count))]
      [(dec y) (mod (->> (/ x ratio) (.floor js/Math))
                    prev-count)])))

(defn polar-cell-outward [{:keys [rows]} [y x]]
  (cond
    (= y 0) (set (make-polar-row 1 (polar-count-row-cells rows 1)))
    (>= y (dec rows)) #{}
    :else (let [next-count (polar-count-row-cells rows (inc y))
                ratio (/ next-count (polar-count-row-cells rows y))]
            (if (= ratio 1)
              #{[(inc y) x]}
              (set (for [c (range ratio)]
                     [(inc y) (-> (* ratio x) (+ c))]))))))

(defrecord PolarGrid [rows links]
  IHaveCells
  (cells-seq [_]
    (let [rows (for [y (range rows)]
                 (if (= 0 y)
                   [[0 0]]
                   (make-polar-row y (polar-count-row-cells rows y))))]
      (reduce concat rows)))

  (valid-pos? [_ [y x]]
    (and (>= y 0) (< y rows)
         (>= x 0) (< x (polar-count-row-cells rows y))))

  (count-cells [this] (count (cells-seq this)))

  (rand-cell [this] (rand-nth (vec (cells-seq this))))

  (cell-neighbors [this cell]
    (if (= cell [0 0])
      (set (make-polar-row 1 (polar-count-row-cells rows 1)))
      (into #{(polar-cell-inward this cell)
              (polar-cell-cw this cell)
              (polar-cell-ccw this cell)}
            (polar-cell-outward this cell))))

  IGridSerialize
  (grid-type-key [_] ::polar))

(defn make-polar-grid [rows]
  (PolarGrid. rows {}))

(defmethod unserialize-grid* ::polar [attrs] (merge (make-polar-grid 1) attrs))

;; common grid functions

(defn visit-cell [grid cell]
  (update-in grid [:links cell] #(or % #{})))

(defn visited-cell? [grid cell]
  (contains? (:links grid) cell))

(defn link-cells [grid cell-a cell-b]
  (assert (not (nil? cell-a)))
  (assert (not (nil? cell-b)))
  (-> grid
      (update-in [:links cell-a] #(conj (or % #{}) cell-b))
      (update-in [:links cell-b] #(conj (or % #{}) cell-a))))

(defn link-path [grid path]
  (reduce (fn [grid [ca cb]] (link-cells grid ca cb))
          grid
          (partition 2 1 path)))

(defn linked-to? [grid cell-a cell-b]
  (contains? (get-in grid [:links cell-a] #{}) cell-b))

(defn unvisited-cells [{:keys [links] :as grid}]
  (->> (cells-seq grid)
       (remove (partial contains? links))))

(defn rows-seq [grid]
  (->> (cells-seq grid) (group-by first) (vals)))

(defn valid-neighbors [grid cell]
  (->> (cell-neighbors grid cell)
       (filter (partial valid-pos? grid))
       (set)))

(defn accessible-neighbors [grid cell]
  (->> (cell-neighbors grid cell)
       (filter (partial linked-to? grid cell))
       (set)))

(defn unvisited-neighbors [grid cell]
  (->> (valid-neighbors grid cell)
       (remove (partial visited-cell? grid))
       (set)))

(defn dead-ends [{:keys [links]}] (filter (fn [[_ v]] (= (count v) 1)) links))

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

(defn gen-aldous-broder [grid]
  (let [cells-n (count-cells grid)]
    (loop [{:keys [links] :as grid} grid
           cell (rand-cell grid)]
      (if (< (count links) cells-n)
        (let [next (rand-nth (vec (valid-neighbors grid cell)))]
          (recur (if (contains? links next) grid (link-cells grid cell next))
                 next))
        grid))))

(defn gen-wilson [grid]
  (let [cells-n (count-cells grid)]
    (loop [{:keys [links] :as grid} (visit-cell grid (rand-cell grid))
           path []]
      (let [path-last (last path)]
        (cond
          (= (count links) cells-n) grid

          ; clean path, pick a random position to start
          (not (seq path)) (recur grid [(rand-nth (vec (unvisited-cells grid)))])

          ; hit a visited place, apply the path
          (contains? links path-last) (recur (link-path grid path) [])

          :else
          (let [next (rand-nth (vec (valid-neighbors grid path-last)))]
            (if-let [self-hit (index-of next path)]
              (recur grid (subvec path 0 (inc self-hit)))
              (recur grid (conj path next)))))))))

(defn gen-hunt-and-kill [grid]
  (let [cells-n (count-cells grid)]
    (loop [{:keys [links] :as grid} grid
           cell (rand-cell grid)]
      (if (= (count links) cells-n)
        grid
        (if-let [next-options (seq (unvisited-neighbors grid cell))]
          (let [next (rand-nth next-options)]
            (recur (link-cells grid cell next) next))
          (let [[next linkable-neighbors]
                (->> (unvisited-cells grid)
                     (keep (fn [c]
                             (let [neighbors (valid-neighbors grid c)
                                   connections (set/intersection links neighbors)]
                               (if (seq connections)
                                 [c (vec connections)]))))
                     first)]
            (recur (link-cells grid next (rand-nth linkable-neighbors)) next)))))))

(defn gen-recursive-backtracker [grid]
  (let [cells-n (count-cells grid)]
    (loop [{:keys [links] :as grid} grid
           stack (list (rand-cell grid))]
      (if (= (count links) cells-n)
        grid
        (let [cell (peek stack)]
          (if-let [next-options (seq (unvisited-neighbors grid cell))]
            (let [next (rand-nth next-options)]
              (recur (link-cells grid cell next) (conj stack next)))
            (recur grid (pop stack))))))))

;; solvers

(defn dijkstra-enumerate [grid start-cell]
  (loop [marks {}
         queue [[start-cell 0]]]
    (if (seq queue)
      (let [[cell distance] (first queue)
            neighbors (->> (accessible-neighbors grid cell)
                           (remove (partial contains? marks))
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

(defn farthest-point [marks] (-> (sort-by second marks) last first))

(defn longest-path-marks [grid]
  (->> (dijkstra-enumerate grid [0 0])
       (farthest-point)
       (dijkstra-enumerate grid)))

(defn longest-path [grid]
  (let [longest-enum (longest-path-marks grid)]
    (trace-route-back grid longest-enum (farthest-point longest-enum))))

;; output

(defn ascii-grid
  ([grid] (ascii-grid grid (fn [_ _] " ")))
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

; other

(defn ascii-mask [& lines]
  (->> (map-indexed (fn [row line] (map-indexed #(if (= "X" %2) [row %]) line))
                    lines)
       (flatten1)
       (filter identity)
       (set)))
