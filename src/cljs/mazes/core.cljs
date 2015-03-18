(ns ^:figwheel-always mazes.core
  (:require-macros [wilkerdev.util.macros :refer [bench]])
  (:require [clojure.string :as str]))

(enable-console-print!)

;; util

(defn index-of [value coll]
  (first (keep-indexed #(if (= %2 value) %1) coll)))

;; grid

(defn make-grid [rows columns]
  {:rows rows :columns columns :links {}})

(defn count-cells [{:keys [rows columns]}] (* rows columns))

(defn visit-cell [grid cell]
  (update-in grid [:links cell] #(or % #{})))

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

(defn unvisited-cells [{:keys [links] :as grid}]
  (->> (cells-seq grid)
       (remove (partial contains? links))))

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

(defn gen-aldous-broder [grid]
  (let [cells-n (count-cells grid)]
    (loop [{:keys [links] :as grid} grid
           cell (rand-cell grid)]
      (if (< (count links) cells-n)
        (let [next (rand-nth (->> (cell-neighbors cell)
                                  (filter (partial valid-pos? grid))))]
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
          (not (seq path)) (recur grid [(rand-nth (unvisited-cells grid))])

          ; hit a visited place, apply the path
          (contains? links path-last) (recur (link-path grid path) [])

          :else
          (let [next (rand-nth (->> (cell-neighbors path-last)
                                    (filter (partial valid-pos? grid))))]
            (if-let [self-hit (index-of next path)]
              (recur grid (subvec path 0 (inc self-hit)))
              (recur grid (conj path next)))))))))

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
