(ns mazes.core-test
  (:require-macros [cemerick.cljs.test
                      :refer (is deftest with-test testing test-var)])
  (:require [cemerick.cljs.test :as t]
            [mazes.core :as m]))

(def grid44 (m/blank-grid 4 4))

(deftest test-blank-grid
  (is (= (m/blank-grid 4 3)
         {:rows 4 :columns 3 :links {}})))

(deftest test-link-cells
  (is (= (m/link-cells (m/blank-grid 4 3) [0 0] [0 1])
         {:rows 4 :columns 3 :links {[0 0] #{[0 1]}
                                     [0 1] #{[0 0]}}}))
  (is (thrown-with-msg? js/Error #"cell-a" (m/link-cells grid44 nil [1 3])))
  (is (thrown-with-msg? js/Error #"cell-b" (m/link-cells grid44 [1 2] nil))))

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
  (is (= (m/cells-seq (m/blank-grid 2 2))
         [[0 0] [0 1] [1 0] [1 1]])))

(deftest test-rows-seq
  (is (= (m/rows-seq (m/blank-grid 2 2))
         [[[0 0] [0 1]] [[1 0] [1 1]]])))

(deftest test-east  (is (= (m/east [0 0]) [0 1])))
(deftest test-north (is (= (m/north [1 0]) [0 0])))
(deftest test-south (is (= (m/south [0 0]) [1 0])))

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

(t/test-ns 'mazes.core-test)
