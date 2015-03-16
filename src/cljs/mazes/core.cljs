(ns ^:figwheel-always mazes.core
    (:require [clojure.string :as str]))

(enable-console-print!)

(defn blank-grid [rows columns]
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

(defn east [[y x]] [y (inc x)])
(defn north [[y x]] [(dec y) x])
(defn south [[y x]] [(inc y) x])

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

(defn ascii-grid [{:keys [columns rows] :as grid}]
  (let [str-repeat #(str/join "" (repeat %1 %2))
        header (str "+" (str-repeat columns "---+") "\n")
        lines (for [row (range rows)
                    :let [parts (for [column (range columns)
                                      :let [cell [row column]]]
                                  [(str "   " (if (linked-to? grid cell (east cell)) " " "|"))
                                   (str (if (linked-to? grid cell (south cell)) "   " "---") "+")])]]
                (str "|" (str/join "" (map first parts)) "\n"
                     "+" (str/join "" (map second parts)) "\n"))]
    (apply str header lines)))
