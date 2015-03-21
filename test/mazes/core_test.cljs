(ns mazes.core-test
  (:require-macros [cemerick.cljs.test
                      :refer (is deftest with-test testing test-var done)]
                   [cljs.core.async.macros :refer [go]])
  (:require [cemerick.cljs.test :as t]
            [mazes.core :as m]
            [mazes.playground :as p]
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

(def masked-maze
  (-> (m/make-grid 3 3)
      (assoc :mask #{[1 1]})))

(deftest test-make-grid
  (is (= (into {} (m/make-grid {:rows 2 :columns 5 :links {[0 0] #{}} :mask #{[1 2]}}))
         {:rows 2 :columns 5 :links {[0 0] #{}} :mask #{[1 2]}}))
  (is (= (into {} (m/make-grid 4 3))
         {:rows 4 :columns 3 :links {} :mask #{}})))

(deftest test-east  (is (= (m/east  [0 0]) [0 1])))
(deftest test-west  (is (= (m/west  [0 1]) [0 0])))
(deftest test-north (is (= (m/north [1 0]) [0 0])))
(deftest test-south (is (= (m/south [0 0]) [1 0])))

(deftest test-valid-pos?
  (is (true? (m/valid-pos? grid44 [0 0])))
  (is (true? (m/valid-pos? grid44 [1 1])))
  (is (true? (m/valid-pos? grid44 [3 3])))
  (is (false? (m/valid-pos? grid44 [-1 0])))
  (is (false? (m/valid-pos? grid44 [0 -1])))
  (is (false? (m/valid-pos? grid44 [0 4])))
  (is (false? (m/valid-pos? grid44 [4 3])))
  (is (false? (m/valid-pos? masked-maze [1 1]))))

(deftest test-cells-seq
  (is (= (m/cells-seq (m/make-grid 2 2))
         [[0 0] [0 1] [1 0] [1 1]]))
  (is (= (m/cells-seq masked-maze)
         [[0 0] [0 1] [0 2]
          [1 0]       [1 2]
          [2 0] [2 1] [2 2]])))

(deftest test-count-cells
  (is (= (m/count-cells grid44) 16))
  (is (= (m/count-cells simple-maze) 9))
  (is (= (m/count-cells masked-maze) 8))
  (is (= (m/count-cells (update masked-maze :mask conj [10 10])) 8)))

(deftest test-rand-cell
  (let [[y x] (m/rand-cell grid44)]
    (is (and (>= y 0) (< y 4)))
    (is (and (>= x 0) (< x 4)))
    (is (false? (contains? (set (repeatedly 100 (partial m/rand-cell masked-maze)))
                           [1 1])))))

(deftest test-cell-neighbors
  (is (= (m/cell-neighbors grid44 [1 1]) #{[0 1] [1 2] [2 1] [1 0]})))

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
          [[1 0]       [1 2]]
          [[2 0] [2 1] [2 2]]])))

(deftest test-unvisited-cells
  (is (= (m/unvisited-cells (assoc (m/make-grid 2 2) :links {[1 1] #{} [0 1] #{}}))
         [[0 0] [1 0]])))

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

(deftest test-unvisited-neighbors
  (is (= (m/unvisited-neighbors (assoc grid44 :links {[0 1] #{}}) [0 0])
         #{[1 0]})))

(deftest test-dead-ends
  (is (= (m/dead-ends simple-maze) [[[2 2] #{[2 1]}] [[2 0] #{[1 0]}]])))

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

(deftest test-ascii-mask
  (is (= (m/ascii-mask "X...X"
                       "..X.."
                       "X...X")
         #{[0 0] [0 4] [1 2] [2 0] [2 4]})))

;; front-end

(def png-mask-example "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAADCAIAAADUVFKvAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyhpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMDE0IDc5LjE1Njc5NywgMjAxNC8wOC8yMC0wOTo1MzowMiAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIDIwMTQgKE1hY2ludG9zaCkiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6MzJFQ0Y3MDdDNzI3MTFFNDlCN0FEQ0RDNzIwRjY2MjgiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6MzJFQ0Y3MDhDNzI3MTFFNDlCN0FEQ0RDNzIwRjY2MjgiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDozMkVDRjcwNUM3MjcxMUU0OUI3QURDREM3MjBGNjYyOCIgc3RSZWY6ZG9jdW1lbnRJRD0ieG1wLmRpZDozMkVDRjcwNkM3MjcxMUU0OUI3QURDREM3MjBGNjYyOCIvPiA8L3JkZjpEZXNjcmlwdGlvbj4gPC9yZGY6UkRGPiA8L3g6eG1wbWV0YT4gPD94cGFja2V0IGVuZD0iciI/Pg0S8McAAAAcSURBVHjaYmRgYPj//z8DGDACAYSDYKDJAwQYADsYDAFTzy+xAAAAAElFTkSuQmCC")

(deftest ^:async test-png-mask
  (go
    (is (= (<! (p/png-mask png-mask-example))
           {:rows 3 :columns 5 :mask #{[0 0] [0 4] [1 2] [2 0] [2 4]}}))
    (done)))

;; testing serialization

(deftest test-serialization
  (is (= (m/serialize-grid grid44)
         {:rows 4 :columns 4 :links {} :mask #{} :grid-type :mazes.core/rectangular}))
  (let [grid (m/unserialize-grid {:rows 4 :columns 4 :links {} :mask #{} :grid-type :mazes.core/rectangular})]
    (is (= (into {} grid) (into {} grid44)))
    (is (instance? m/RectangularGrid grid))))

(t/test-ns 'mazes.core-test)
