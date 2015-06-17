(ns ^:figwheel-always mazes.core-test
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.test :refer-macros [is are deftest run-tests]]
            [mazes.core :as m]
            [cljs.core.async :refer [<!]]))

;; testing utilities

(deftest test-index-of
  (is (= (m/index-of :a [:a :b :c]) 0))
  (is (= (m/index-of :b [:a :b :c]) 1))
  (is (= (m/index-of :c [:a :b :c]) 2))
  (is (= (m/index-of :d [:a :b :c]) nil)))

;; testing rectangular grid

(def grid44 (m/make-grid 4 4))

; +---+---+---+
; |           |
; +   +---+   +
; |   |       |
; +   +   +---+
; |   |       |
; +---+---+---+
(def simple-maze
  (-> (m/make-grid 3 3)
      (assoc :links {[0 1] #{[0 0] [0 2]} [1 2] #{[1 1] [0 2]} [0 0] #{[0 1] [1 0]} [2 2] #{[2 1]}
                     [0 2] #{[0 1] [1 2]} [1 1] #{[1 2] [2 1]} [2 1] #{[2 2] [1 1]}
                     [1 0] #{[0 0] [2 0]} [2 0] #{[1 0]}})))

(deftest test-make-grid
  (is (= (into {} (m/make-grid {:rows 2 :columns 5 :links {[0 0] #{}}}))
         {:rows 2 :columns 5 :links {[0 0] #{}}}))
  (is (= (into {} (m/make-grid 4 3))
         {:rows 4 :columns 3 :links {}})))

(deftest test-east (is (= (m/east [0 0]) [0 1])))
(deftest test-west (is (= (m/west [0 1]) [0 0])))
(deftest test-north (is (= (m/north [1 0]) [0 0])))
(deftest test-south (is (= (m/south [0 0]) [1 0])))

(deftest test-valid-pos?
  (are [pos pred] (= (m/valid-pos? grid44 pos) pred)
    [0 0]  true
    [1 1]  true
    [3 3]  true
    [-1 0] false
    [0 -1] false
    [0 4]  false
    [4 3]  false))

(deftest test-cells-seq
  (is (= (m/cells-seq (m/make-grid 2 2))
         [[0 0] [0 1] [1 0] [1 1]])))

(deftest test-count-cells
  (is (= (m/count-cells grid44) 16))
  (is (= (m/count-cells simple-maze) 9)))

(deftest test-rand-cell
  (let [[y x] (m/rand-cell grid44)]
    (is (and (>= y 0) (< y 4)))
    (is (and (>= x 0) (< x 4)))))

(deftest test-cell-neighbors
  (is (= (m/cell-neighbors grid44 [1 1]) #{[0 1] [1 2] [2 1] [1 0]})))

;; testing masked grid

(def masked-maze
  (-> (m/make-grid 3 3)
      (m/masked-grid #{[1 1]})))

(deftest test-masked-valid-pos?
  (is (false? (m/valid-pos? masked-maze [1 1]))))

(deftest test-masked-cells-seq
  (is (= (m/cells-seq masked-maze)
         [[0 0] [0 1] [0 2]
          [1 0] [1 2]
          [2 0] [2 1] [2 2]])))

(deftest test-masked-count-cells
  (is (= (m/count-cells masked-maze) 8))
  (is (= (m/count-cells (update masked-maze :mask conj [10 10])) 8)))

(deftest test-masked-rand-cell
  (is (false? (contains? (set (repeatedly 100 (partial m/rand-cell masked-maze)))
                         [1 1]))))

;; common grid methods

(deftest test-visit-cell
  (is (= (m/visit-cell {:links {}} [0 1])
         {:links {[0 1] #{}}}))
  (is (= (m/visit-cell {:links {[0 1] #{[0 2]}}} [0 1])
         {:links {[0 1] #{[0 2]}}})))

(deftest test-visited-cell?
  (is (true? (m/visited-cell? {:links {[0 0] #{}}} [0 0])))
  (is (false? (m/visited-cell? {:links {[0 0] #{}}} [0 1]))))

(deftest test-link-cells
  (is (= (:links (m/link-cells (m/make-grid 4 3) [0 0] [0 1]))
         {[0 0] #{[0 1]}
          [0 1] #{[0 0]}}))
  (is (thrown-with-msg? js/Error #"cell-a" (m/link-cells grid44 nil [1 3])))
  (is (thrown-with-msg? js/Error #"cell-b" (m/link-cells grid44 [1 2] nil))))

(deftest test-link-path
  (is (= (:links (m/link-path grid44 [[0 0] [0 1] [1 1] [2 1]]))
         {[0 0] #{[0 1]}
          [0 1] #{[0 0] [1 1]}
          [1 1] #{[0 1] [2 1]}
          [2 1] #{[1 1]}})))

(deftest test-linked-to?
  (let [linked-grid (m/link-cells grid44 [1 2] [1 3])]
    (is (true? (m/linked-to? linked-grid [1 2] [1 3])))
    (is (true? (m/linked-to? linked-grid [1 2] [1 3])))
    (is (false? (m/linked-to? linked-grid [1 2] [1 1])))
    (is (false? (m/linked-to? linked-grid nil [1 1])))
    (is (false? (m/linked-to? linked-grid [1 2] nil)))))

(deftest test-rows-seq
  (is (= (m/rows-seq (m/make-grid 2 2))
         [[[0 0] [0 1]] [[1 0] [1 1]]]))
  (is (= (m/rows-seq masked-maze)
         [[[0 0] [0 1] [0 2]]
          [[1 0] [1 2]]
          [[2 0] [2 1] [2 2]]])))

(deftest test-unvisited-cells
  (is (= (m/unvisited-cells (assoc (m/make-grid 2 2) :links {[1 1] #{} [0 1] #{}}))
         [[0 0] [1 0]])))

(deftest test-valid-neighbors
  (is (= (m/valid-neighbors grid44 [0 0]) #{[0 1] [1 0]}))
  (is (= (m/valid-neighbors grid44 [1 0]) #{[0 0] [1 1] [2 0]}))
  (is (= (m/valid-neighbors grid44 [1 1]) #{[0 1] [1 2] [2 1] [1 0]})))

(deftest test-accessible-neighbors
  (are [pos neighbors] (= (m/accessible-neighbors simple-maze pos) neighbors)
    [0 0] #{[0 1] [1 0]}
    [0 1] #{[0 2] [0 0]}
    [0 2] #{[1 2] [0 1]}
    [1 0] #{[0 0] [2 0]}
    [1 1] #{[1 2] [2 1]}
    [1 2] #{[0 2] [1 1]}
    [2 0] #{[1 0]}
    [2 1] #{[1 1] [2 2]}
    [2 2] #{[2 1]}))

(deftest test-unvisited-neighbors
  (is (= (m/unvisited-neighbors (assoc grid44 :links {[0 1] #{}}) [0 0])
         #{[1 0]})))

(deftest test-unlinked-neighbors
  (are [pos neighbors] (= (m/unlinked-neighbors simple-maze pos) neighbors)
    [0 0] #{}
    [0 1] #{[1 1]}
    [0 2] #{}
    [1 0] #{[1 1]}
    [1 1] #{[0 1] [1 0]}
    [1 2] #{[2 2]}
    [2 0] #{[2 1]}
    [2 1] #{[2 0]}))

(deftest test-dead-end?
  (are [pos dead?] (= (m/dead-end? simple-maze pos) dead?)
    [0 0] false
    [2 0] true
    [2 1] false
    [2 2] true))

(deftest test-dead-ends
  (is (= (m/dead-ends simple-maze) #{[2 0] [2 2]})))

; +---+---+---+
; |           |
; +   +---+   +
; |   |       |
; +   +   +   +
; |           |
; +---+---+---+
(deftest test-braid
  (is (= (:links (m/braid simple-maze 1))
         {[0 0] #{[1 0] [0 1]}, [0 1] #{[0 0] [0 2]},       [0 2] #{[1 2] [0 1]}
          [1 0] #{[0 0] [2 0]}, [1 1] #{[2 1] [1 2]},       [1 2] #{[1 1] [0 2] [2 2]}
          [2 0] #{[1 0] [2 1]}, [2 1] #{[2 2] [1 1] [2 0]}, [2 2] #{[2 1] [1 2]}}))
  (is (= (:links (m/braid simple-maze 0))
         (:links simple-maze))))

(deftest test-binary-tree-cell
  (is (= (m/binary-tree-link-cell grid44 [0 3] :north) nil))
  (is (= (m/binary-tree-link-cell grid44 [0 0] :north) [0 1]))
  (is (= (m/binary-tree-link-cell grid44 [0 1] :east) [0 2]))
  (is (= (m/binary-tree-link-cell grid44 [2 2] :east) [2 3]))
  (is (= (m/binary-tree-link-cell grid44 [2 2] :north) [1 2]))
  (is (= (m/binary-tree-link-cell grid44 [3 0] :north) [2 0]))
  (is (= (m/binary-tree-link-cell grid44 [3 0] :east) [3 1]))
  (is (= (m/binary-tree-link-cell grid44 [3 3] :north) [2 3]))
  (is (= (m/binary-tree-link-cell grid44 [3 3] :east) [2 3])))

(deftest test-dijkstra-enumerate
  ; +---+---+---+
  ; | 0   1   2 |
  ; +   +---+   +
  ; | 1 | 4   3 |
  ; +   +   +---+
  ; | 2 | 5   6 |
  ; +---+---+---+
  (is (= (m/dijkstra-enumerate simple-maze [0 0])
         {[0 0] 0 [0 1] 1 [0 2] 2
          [1 0] 1 [1 1] 4 [1 2] 3
          [2 0] 2 [2 1] 5 [2 2] 6}))

  ; +---+---+---+
  ; | 2   1   0 |
  ; +   +---+   +
  ; | 3 | 2   1 |
  ; +   +   +---+
  ; | 4 | 3   4 |
  ; +---+---+---+
  (is (= (m/dijkstra-enumerate simple-maze [0 2])
         {[0 0] 2 [0 1] 1 [0 2] 0
          [1 0] 3 [1 1] 2 [1 2] 1
          [2 0] 4 [2 1] 3 [2 2] 4}))

  ; +---+---+---+
  ; | 4   3   2 |
  ; +   +---+   +
  ; | 5 | 0   1 |
  ; +   +   +---+
  ; | 6 | 1   2 |
  ; +---+---+---+
  (is (= (m/dijkstra-enumerate simple-maze [1 1])
         {[0 0] 4 [0 1] 3 [0 2] 2
          [1 0] 5 [1 1] 0 [1 2] 1
          [2 0] 6 [2 1] 1 [2 2] 2}))

  ; +----+---+---+
  ; |  0   1   2 |
  ; +    +---+   +
  ; | 50 | 4   3 |
  ; +    +   +---+
  ; |  6   5   6 |
  ; +----+---+---+
  (is (= (m/dijkstra-enumerate (-> simple-maze
                                   (m/link-cells [2 0] [2 1])
                                   (assoc :cell-costs {[1 0] 50})) [0 0])
         {[0 0] 0  [0 1] 1 [0 2] 2
          [1 0] 50 [1 1] 4 [1 2] 3
          [2 0] 6  [2 1] 5 [2 2] 6})))

; +---+---+---+
; | 0   1   2 |
; +   +---+   +
; | 1 | 4   3 |
; +   +   +---+
; | 2 | 5   6 |
; +---+---+---+
(def sample-enumeration
  {[0 0] 0 [0 1] 1 [0 2] 2
   [1 0] 1 [1 1] 4 [1 2] 3
   [2 0] 2 [2 1] 5 [2 2] 6})

(deftest test-trace-route-back
  (is (= (m/trace-route-back simple-maze sample-enumeration [1 1])
         [[1 1] [1 2] [0 2] [0 1] [0 0]])))

(deftest test-farthest-path
  (is (= (m/farthest-point sample-enumeration)
         [2 2])))

(deftest test-longest-path-enum
  (is (= (m/longest-path-marks simple-maze)
         {[0 1] 5, [1 2] 3, [0 0] 6, [2 2] 0, [0 2] 4, [1 1] 2, [2 1] 1, [1 0] 7, [2 0] 8})))

(deftest test-longest-path
  (is (= (m/longest-path simple-maze)
         [[2 0] [1 0] [0 0] [0 1] [0 2] [1 2] [1 1] [2 1] [2 2]])))

(deftest test-ascii-mask
  (is (= (m/ascii-mask "X...X"
                       "..X.."
                       "X...X")
         #{[0 0] [0 4] [1 2] [2 0] [2 4]})))

;; polar grid

(deftest test-polar-cell-cw
  (let [grid (m/make-polar-grid 8)]
    (is (= (m/polar-cell-cw grid [0 0]) [0 0]))
    (is (= (m/polar-cell-cw grid [1 0]) [1 1]))
    (is (= (m/polar-cell-cw grid [1 1]) [1 2]))
    (is (= (m/polar-cell-cw grid [1 5]) [1 0]))))

(deftest test-polar-cell-ccw
  (let [grid (m/make-polar-grid 8)]
    (is (= (m/polar-cell-ccw grid [0 0]) [0 0]))
    (is (= (m/polar-cell-ccw grid [1 0]) [1 5]))
    (is (= (m/polar-cell-ccw grid [1 1]) [1 0]))
    (is (= (m/polar-cell-ccw grid [1 5]) [1 4]))))

(deftest test-polar-cell-inward
  (let [grid (m/make-polar-grid 8)]
    (is (= (m/polar-cell-inward grid [0 0]) [-1 0]))
    (is (= (m/polar-cell-inward grid [1 0]) [0 0]))
    (is (= (m/polar-cell-inward grid [1 1]) [0 0]))
    (is (= (m/polar-cell-inward grid [1 2]) [0 0]))
    (is (= (m/polar-cell-inward grid [1 3]) [0 0]))
    (is (= (m/polar-cell-inward grid [2 0]) [1 0]))
    (is (= (m/polar-cell-inward grid [2 1]) [1 0]))
    (is (= (m/polar-cell-inward grid [2 2]) [1 1]))))

(deftest test-polar-cell-outward
  (let [grid (m/make-polar-grid 8)]
    (is (= (m/polar-cell-outward grid [0 0]) #{[1 0] [1 1] [1 2] [1 3] [1 4] [1 5]}))
    (is (= (m/polar-cell-outward grid [1 0]) #{[2 0] [2 1]}))
    (is (= (m/polar-cell-outward grid [1 1]) #{[2 2] [2 3]}))
    (is (= (m/polar-cell-outward grid [3 0]) #{[4 0]}))
    (is (= (m/polar-cell-outward grid [7 0]) #{}))))

(deftest test-polar-cells-seq
  (let [grid (m/make-polar-grid 8)]
    (is (= (m/cells-seq grid)
           (concat (m/make-polar-row 0 1)
                   (m/make-polar-row 1 6)
                   (m/make-polar-row 2 12)
                   (m/make-polar-row 3 24)
                   (m/make-polar-row 4 24)
                   (m/make-polar-row 5 24)
                   (m/make-polar-row 6 48)
                   (m/make-polar-row 7 48))))))

(deftest test-polar-valid-pos
  (let [grid (m/make-polar-grid 8)]
    (are [pos pred] (= (m/valid-pos? grid pos) pred)
      [0 0]  true
      [1 1]  true
      [7 45] true
      [-1 0] false
      [0 1]  false
      [1 10] false)))

(deftest test-polar-count-cells
  (let [grid (m/make-polar-grid 8)]
    (is (= (m/count-cells grid) 187))))

(deftest test-polar-cell-neighbors
  (let [grid (m/make-polar-grid 8)]
    (is (= (m/cell-neighbors grid [0 0])
           #{[1 0] [1 1] [1 2] [1 3] [1 4] [1 5]}))
    (is (= (m/cell-neighbors grid [1 0])
           #{[0 0] [1 5] [1 1] [2 0] [2 1]}))))

;; hex grid

(deftest test-northeast
  (are [c n] (= (m/northeast c) n)
    [2 0] [1 1]
    [1 1] [1 2]))

(deftest test-northwest
  (are [c n] (= (m/northwest c) n)
    [1 1] [1 0]
    [1 2] [0 1]))

(deftest test-southeast
  (are [c n] (= (m/southeast c) n)
    [2 0] [2 1]
    [2 1] [3 2]))

(deftest test-southwest
  (are [c n] (= (m/southwest c) n)
    [2 2] [2 1]
    [2 1] [3 0]))

(deftest test-hex-cells-seq
  (let [grid (m/make-hex-grid 3 3)]
    (is (= (m/cells-seq grid) [[0 0] [0 1] [0 2]
                               [1 0] [1 1] [1 2]
                               [2 0] [2 1] [2 2]]))))

(deftest test-hex-cell-neighbors
  (are [cell neighbors] (= (m/cell-neighbors (m/make-hex-grid 5 5) cell) neighbors)
    [1 1] #{[1 0] [0 1] [1 2] [2 2] [2 1] [2 0]}
    [2 1] #{[2 0] [1 1] [2 2] [3 2] [3 1] [3 0]}))

;; triangle grid

(deftest test-triangle-upright?
  (are [cell upright?] (= (m/cell-upright? cell) upright?)
    [0 0] true
    [0 1] false
    [0 2] true
    [1 0] false
    [1 1] true
    [1 2] false))

(deftest test-triangle-cell-neighbors
  (are [cell neighbors] (= (m/cell-neighbors (m/make-triangle-grid 5 5) cell) neighbors)
    [0 2] #{[0 1] [0 3] [1 2]}
    [1 2] #{[1 1] [1 3] [0 2]}))

(comment
  (run-tests 'mazes.core-test))
