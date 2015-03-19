(ns mazes.core-test
  (:require-macros [cemerick.cljs.test
                      :refer (is deftest with-test testing test-var)])
  (:require [cemerick.cljs.test :as t]
            [mazes.core :as m]))

(def grid44 (m/make-grid 4 4))

; +---+---+---+
; |           |
; +   +---+   +
; |   |       |
; +   +   +---+
; |   |       |
; +---+---+---+
(def simple-maze
  {:rows 3, :columns 3
   :links {[0 1] #{[0 0] [0 2]} [1 2] #{[1 1] [0 2]} [0 0] #{[0 1] [1 0]} [2 2] #{[2 1]}
           [0 2] #{[0 1] [1 2]} [1 1] #{[1 2] [2 1]} [2 1] #{[2 2] [1 1]}
           [1 0] #{[0 0] [2 0]} [2 0] #{[1 0]}}})

(deftest test-index-of
  (is (= (m/index-of :a [:a :b :c]) 0))
  (is (= (m/index-of :b [:a :b :c]) 1))
  (is (= (m/index-of :c [:a :b :c]) 2))
  (is (= (m/index-of :d [:a :b :c]) nil)))

(deftest test-make-grid
  (is (= (m/make-grid 4 3)
         {:rows 4 :columns 3 :links {}})))

(deftest test-count-cells
  (is (= (m/count-cells grid44) 16))
  (is (= (m/count-cells simple-maze) 9)))

(deftest test-visit-cell
  (is (= (m/visit-cell {:links {}} [0 1])
         {:links {[0 1] #{}}}))
  (is (= (m/visit-cell {:links {[0 1] #{[0 2]}}} [0 1])
         {:links {[0 1] #{[0 2]}}})))

(deftest test-visited-cell?
  (is (true? (m/visited-cell? {:links {[0 0] #{}}} [0 0])))
  (is (false? (m/visited-cell? {:links {[0 0] #{}}} [0 1]))))

(deftest test-link-cells
  (is (= (m/link-cells (m/make-grid 4 3) [0 0] [0 1])
         {:rows 4 :columns 3 :links {[0 0] #{[0 1]}
                                     [0 1] #{[0 0]}}}))
  (is (thrown-with-msg? js/Error #"cell-a" (m/link-cells grid44 nil [1 3])))
  (is (thrown-with-msg? js/Error #"cell-b" (m/link-cells grid44 [1 2] nil))))

(deftest test-link-path
  (is (= (m/link-path grid44 [[0 0] [0 1] [1 1] [2 1]])
         {:rows 4, :columns 4, :links {[0 0] #{[0 1]}
                                       [0 1] #{[0 0] [1 1]}
                                       [1 1] #{[0 1] [2 1]}
                                       [2 1] #{[1 1]}}})))

(deftest test-linked-to?
  (let [linked-grid (m/link-cells grid44 [1 2] [1 3])]
    (is (true? (m/linked-to? linked-grid [1 2] [1 3])))
    (is (true? (m/linked-to? linked-grid [1 2] [1 3])))
    (is (false? (m/linked-to? linked-grid [1 2] [1 1])))
    (is (false? (m/linked-to? linked-grid nil [1 1])))
    (is (false? (m/linked-to? linked-grid [1 2] nil)))))

(deftest test-grid-cell
  (is (= (m/cell grid44 [1 2])
         {:x 2 :y 1 :links #{}}))
  (is (= (m/cell grid44 [5 3]) nil))

  (let [linked-grid (m/link-cells grid44 [1 2] [1 3])]
    (is (= (m/cell linked-grid [1 2])
           {:x 2 :y 1 :links #{[1 3]}}))
    (is (= (m/cell linked-grid [1 3])
           {:x 3 :y 1 :links #{[1 2]}}))))

(deftest test-rand-cell
  (let [[y x] (m/rand-cell grid44)]
    (is (and (>= y 0) (< y 4)))
    (is (and (>= x 0) (< x 4)))))

(deftest test-cells-seq
  (is (= (m/cells-seq (m/make-grid 2 2))
         [[0 0] [0 1] [1 0] [1 1]])))

(deftest test-rows-seq
  (is (= (m/rows-seq (m/make-grid 2 2))
         [[[0 0] [0 1]] [[1 0] [1 1]]])))

(deftest test-unvisited-cells
  (is (= (m/unvisited-cells (assoc (m/make-grid 2 2) :links {[1 1] #{} [0 1] #{}}))
         [[0 0] [1 0]])))

(deftest test-east  (is (= (m/east  [0 0]) [0 1])))
(deftest test-west  (is (= (m/west  [0 1]) [0 0])))
(deftest test-north (is (= (m/north [1 0]) [0 0])))
(deftest test-south (is (= (m/south [0 0]) [1 0])))

(deftest test-cell-neighbors
  (is (= (m/cell-neighbors [1 1]) #{[0 1] [1 2] [2 1] [1 0]})))

(deftest test-valid-neighbors
  (is (= (m/valid-neighbors grid44 [0 0]) #{[0 1] [1 0]}))
  (is (= (m/valid-neighbors grid44 [1 0]) #{[0 0] [1 1] [2 0]}))
  (is (= (m/valid-neighbors grid44 [1 1]) #{[0 1] [1 2] [2 1] [1 0]})))

(deftest test-accessible-neighbors
  (is (= (m/accessible-neighbors simple-maze [0 0]) #{[0 1] [1 0]}))
  (is (= (m/accessible-neighbors simple-maze [0 1]) #{[0 2] [0 0]}))
  (is (= (m/accessible-neighbors simple-maze [0 2]) #{[1 2] [0 1]}))
  (is (= (m/accessible-neighbors simple-maze [1 0]) #{[0 0] [2 0]}))
  (is (= (m/accessible-neighbors simple-maze [1 1]) #{[1 2] [2 1]}))
  (is (= (m/accessible-neighbors simple-maze [1 2]) #{[0 2] [1 1]}))
  (is (= (m/accessible-neighbors simple-maze [2 0]) #{[1 0]}))
  (is (= (m/accessible-neighbors simple-maze [2 1]) #{[1 1] [2 2]}))
  (is (= (m/accessible-neighbors simple-maze [2 2]) #{[2 1]})))

(deftest test-binary-tree-cell
  (is (= (m/binary-tree-link-cell grid44 [0 3] :north) nil  ))
  (is (= (m/binary-tree-link-cell grid44 [0 0] :north) [0 1]))
  (is (= (m/binary-tree-link-cell grid44 [0 1] :east)  [0 2]))
  (is (= (m/binary-tree-link-cell grid44 [2 2] :east)  [2 3]))
  (is (= (m/binary-tree-link-cell grid44 [2 2] :north) [1 2]))
  (is (= (m/binary-tree-link-cell grid44 [3 0] :north) [2 0]))
  (is (= (m/binary-tree-link-cell grid44 [3 0] :east)  [3 1]))
  (is (= (m/binary-tree-link-cell grid44 [3 3] :north) [2 3]))
  (is (= (m/binary-tree-link-cell grid44 [3 3] :east)  [2 3])))

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
          [2 0] 6 [2 1] 1 [2 2] 2})))

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

(t/test-ns 'mazes.core-test)
