(ns mazes.playground
  (:require-macros [wilkerdev.util.macros :refer [bench go-sub go-sub*]])
  (:require [mazes.core :refer [cells-seq valid-pos? linked-to? north east south west
                                make-grid rand-cell dijkstra-enumerate farthest-point]
             :as m]
            [om.core :as om]
            [om.dom :as dom]
            [cljs.core.async :refer [chan put!] :as async]))

(defn canvas-line [ctx [x1 y1] [x2 y2]]
  (doto ctx
    (.beginPath)
    (.moveTo x1 y1) (.lineTo x2 y2)
    (.stroke)))

(defn cell-bounds [[y x] cell-size]
  [(* x cell-size) (* y cell-size)
   (* (inc x) cell-size) (* (inc y) cell-size)])

(defn draw-grid [ctx grid cell-size]
  (bench "draw grid"
    (doseq [cell (cells-seq grid) :let [[x1 y1 x2 y2] (cell-bounds cell cell-size)]]
      (if-not (valid-pos? grid (north cell)) (canvas-line ctx [x1 y1] [x2 y1]))
      (if-not (valid-pos? grid (west cell)) (canvas-line ctx [x1 y1] [x1 y2]))
      (if-not (linked-to? grid cell (east cell)) (canvas-line ctx [x2 y1] [x2 y2]))
      (if-not (linked-to? grid cell (south cell)) (canvas-line ctx [x1 y2] [x2 y2])))))

(defn color-compute-blue-to-red [x]
  (let [color (->> (* 255 x)
                   (.round js/Math)
                   (- 255))]
    (str "rgb(" (- 255 color) ", " color ", " color ")")))

(defn color-compute-green-to-black [x]
  (let [color (->> (* 255 x)
                   (.round js/Math)
                   (- 255))]
    (str "rgb(" 0 ", " color ", " 0 ")")))

(defn draw-backgrounds [ctx marks cell-size color-fn]
  (bench "draw background"
    (let [max-distance (get marks (farthest-point marks))]
      (doseq [[cell distance] marks
              :let [[x y] (cell-bounds cell cell-size)]]
        (doto ctx
          (aset "fillStyle" (color-fn (/ distance max-distance)))
          (.fillRect x y cell-size cell-size))))))

;; misc

(defn draw-maze [canvas
                 {:keys [grid-size gen-algorithm mark-builder color-fn]
                  :or   {grid-size     10
                         gen-algorithm m/gen-binary-tree
                         mark-builder  #(dijkstra-enumerate % (rand-cell %))
                         color-fn      color-compute-blue-to-red}}]
  (let [ctx (.getContext canvas "2d")
        grid (bench "generating maze" (-> (make-grid grid-size grid-size) gen-algorithm))
        cell-size (/ (.-width canvas) (:columns grid))
        marks (mark-builder grid)]
    (doto ctx
      (.clearRect 0 0 (.-width canvas) (.-height canvas))
      (.save)
      (draw-backgrounds marks cell-size color-fn)
      (draw-grid grid cell-size)
      (.restore))))

(def opt-algorithms
  {:binary-tree m/gen-binary-tree
   :sidewinder m/gen-sidewinder
   :aldous-broder m/gen-aldous-broder
   :wilson m/gen-wilson})

(defn impl->options [m]
  (->> (keys m)
       (map #(hash-map :label (name %) :value (name %)))))

(defn attrs [base & removals]
  (clj->js (apply dissoc base removals)))

(defn comp-option [{:keys [label value]}] (dom/option #js {:value value} label))

(defn comp-select [{:keys [options] :as attributes}]
  (apply dom/select (attrs attributes :options)
         (map comp-option options)))

(defn comp-grid [{:keys [grid marks] :as attributes} owner]
  (let [render (fn []
                 (let [canvas (om/get-node owner "canvas")
                       ctx (.getContext canvas "2d")
                       cache-backgrounds (om/get-state owner :cache-backgrounds)
                       cache-grid (om/get-state owner :cache-grid)]
                   (bench "rendering from cache"
                     (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
                     (if cache-backgrounds (.drawImage ctx cache-backgrounds 0 0))
                     (if cache-grid (.drawImage ctx cache-grid 0 0)))))
        cache-render (fn [name render-fn]
                       (let [canvas (doto (.createElement js/document "canvas")
                                      (aset "width" 600) (aset "height" 600))
                             ctx (.getContext canvas "2d")
                             cell-size (/ (.-width canvas) (:columns grid))]
                         (render-fn ctx cell-size)
                         (om/set-state! owner name canvas)))
        render-backgrounds (fn []
                             (cache-render :cache-backgrounds
                               #(draw-backgrounds % marks %2 color-compute-blue-to-red)))
        render-grid (fn [] (cache-render :cache-grid #(draw-grid % grid %2)))]
    (reify
      om/IDidMount
      (did-mount [_]
        (render-backgrounds)
        (render-grid)
        (render))
      om/IDidUpdate
      (did-update [_ pp _]
        (if (not= (:grid pp) grid) (render-grid))
        (if (or (not= (:marks pp) marks)
                (not= (:marks pp) marks)) (render-backgrounds))
        (render))
      om/IRender
      (render [_]
        (dom/canvas (-> (assoc attributes :ref "canvas")
                        (attrs :grid)))))))

(defonce app-state
  (atom {:grid-size      10
         :generator      :binary-tree
         :marker-builder :random-point
         :colorizer      :blue-to-red}))

(defn fit-in-range [n mi ma] (max (min n ma) mi))

(defn maze-playground [{:keys [generator grid-size] :as data} owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [bus (chan 1024)
            pub (async/pub bus first)]
        {:bus bus :pub pub}))
    om/IWillMount
    (will-mount [_]
      (let [pub (om/get-state owner :pub)
            bus (om/get-state owner :bus)]

        (go-sub pub :update-generator [_ generator]
          (om/update! data :generator (keyword generator))
          (put! bus [:generate-maze]))

        (go-sub pub :update-grid-size [_ grid-size]
          (let [n (or (js/parseInt grid-size) 0)]
            (om/update! data :grid-size (fit-in-range n 2 100)))
          (put! bus [:generate-maze]))

        (go-sub pub :generate-maze [_]
          (let [grid-size (:grid-size @app-state)
                generator (get opt-algorithms (:generator @app-state))
                maze (bench "generating maze" (-> (m/make-grid grid-size grid-size) generator))
                marks (bench "generating marks" (-> (m/dijkstra-enumerate maze (m/rand-cell maze))))]
            (om/update! data :maze maze)
            (om/update! data :marks marks)))))

    om/IRender
    (render [_]
      (let [bus (om/get-state owner :bus)]
        (dom/div nil
          (dom/label #js {:style #js {:display "block"}}
            "Generator algorithm: "
            (comp-select {:value    (name generator) :options (impl->options opt-algorithms)
                          :onChange #(put! bus [:update-generator (.. % -target -value)])}))
          (dom/label #js {:style #js {:display "block"}}
            "Grid size: "
            (dom/input #js {:type     "number" :value grid-size :min 4 :max 100
                            :onChange #(put! bus [:update-grid-size (.. % -target -value)])}))
          (dom/button #js {:onClick #(put! bus [:generate-maze])
                           :style #js {:margin-top "10px"}} "Generate maze")
          (dom/hr nil)
          (om/build comp-grid {:grid (:maze data) :marks (:marks data)
                               :width 600 :height 600
                               :style #js {:border "1px solid #000"}}))))))

(defn build-at [node]
  (let [root (om/root maze-playground app-state {:target node})]
    (om/get-state root)))
