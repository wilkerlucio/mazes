(ns ^:figwheel-always mazes.playground-test
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.test :refer-macros [is are deftest run-tests async]]
            [mazes.core :as m]
            [mazes.playground :as p]
            [cljs.core.async :refer [<!]]
            [clojure.walk :refer [postwalk]]))

(def png-mask-example "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAADCAIAAADUVFKvAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyhpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMDE0IDc5LjE1Njc5NywgMjAxNC8wOC8yMC0wOTo1MzowMiAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIDIwMTQgKE1hY2ludG9zaCkiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6MzJFQ0Y3MDdDNzI3MTFFNDlCN0FEQ0RDNzIwRjY2MjgiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6MzJFQ0Y3MDhDNzI3MTFFNDlCN0FEQ0RDNzIwRjY2MjgiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDozMkVDRjcwNUM3MjcxMUU0OUI3QURDREM3MjBGNjYyOCIgc3RSZWY6ZG9jdW1lbnRJRD0ieG1wLmRpZDozMkVDRjcwNkM3MjcxMUU0OUI3QURDREM3MjBGNjYyOCIvPiA8L3JkZjpEZXNjcmlwdGlvbj4gPC9yZGY6UkRGPiA8L3g6eG1wbWV0YT4gPD94cGFja2V0IGVuZD0iciI/Pg0S8McAAAAcSURBVHjaYmRgYPj//z8DGDACAYSDYKDJAwQYADsYDAFTzy+xAAAAAElFTkSuQmCC")

(defn round-results [results]
  (postwalk (fn [v] (if (number? v) (.round js/Math v) v)) results))

(deftest test-png-mask
  (async done
    (go
      (is (= (<! (p/png-mask png-mask-example))
             {:rows 3 :columns 5 :mask #{[0 0] [0 4] [1 2] [2 0] [2 4]}}))
      (done))))

;; util

(deftest test-rect-center
  (is (= (p/rect-center {:width 10 :height 20}) [5 10])))

;; testing serialization

#_ (deftest test-serialization
  (let [grid44 (m/make-grid 4 4)]
    (is (= (p/serialize-record grid44)
           {:rows 4 :columns 4 :links {} :mask #{} :grid-type :mazes.playground/rectangular}))
    (let [grid (p/unserialize-record {:rows 4 :columns 4 :links {} :mask #{} :grid-type :mazes.playground/rectangular})]
      (is (= (into {} grid) (into {} grid44)))
      (is (instance? m/RectangularGrid grid)))))

;; hex

(deftest test-hex-measures
  (let [grid (-> (m/make-hex-grid 10 10)
                 (assoc :width 156 :height 183))]
    (is (= (round-results (p/hex-measures grid [0 0]))
           {:x-fw 0 :s 8 :x-ne 12 :ch 14 :y-s 14 :y-n 0 :cx 8 :cy 7 :b 7 :x-fe 16 :x-nw 4
            :edges [4 0 12 0 16 7 12 14 4 14 0 7] :cw 16 :a 4}))))

;; triangle

(deftest test-triangle-measures
  (let [grid (-> (m/make-triangle-grid 10 10)
                 (assoc :width 156 :height 183))]
    (is (= (round-results (p/triangle-measures grid [0 0]))
           {:wx 0 :mx 8 :ex 16 :ay 0 :by 14 :edges [0 14 8 0 16 14]}))))

;; run

(run-tests 'mazes.playground-test)
