(ns mazes.playground
  (:require-macros [wilkerdev.util.macros :refer [bench go-sub go-sub*]]
                   [cljs.core.async.macros :refer [go]])
  (:require [mazes.core :refer [cells-seq valid-pos? linked-to? north east south west
                                make-grid rand-cell dijkstra-enumerate farthest-point]
             :as m]
            [om.core :as om]
            [om.dom :as dom]
            [cljs.core.async :refer [chan put!] :as async]))

;; state and data

(defonce app-state
  (atom {:grid-size      10
         :generator      :recursive-backtracker
         :marker-builder :random-point
         :colorizer      :blue-to-red
         :layers         {:distance-mash {:show     true
                                          :color-fn :blue-to-red}
                          :grid-lines    {:show true}
                          :dead-ends     {:show true}}}))

(def opt-algorithms
  (sorted-map
    :aldous-broder {:label "Aldous Broder"
                    :value m/gen-aldous-broder}
    :binary-tree {:label "Binary Tree"
                  :value m/gen-binary-tree}
    :hunt-and-kill {:label "Hunt and Kill"
                    :value m/gen-hunt-and-kill}
    :recursive-backtracker {:label "Recursive Backtracker"
                            :value m/gen-recursive-backtracker}
    :sidewinder {:label "Sidewinder"
                 :value m/gen-sidewinder}
    :wilson {:label "Wilson's"
             :value m/gen-wilson}))

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

(def opt-color-functions
  (sorted-map
    :blue-to-red {:label "Blue to Red"
                  :value color-compute-blue-to-red}
    :green-to-black {:label "Green to Black"
                     :value color-compute-green-to-black}))

;; helpers

(defn cell-bounds [[y x] cell-size]
  [(* x cell-size) (* y cell-size)
   (* (inc x) cell-size) (* (inc y) cell-size)])

(defn impl->options [m]
  (map (fn [[k v]] {:label (:label v) :value (name k)}) m))

(defn attrs [base & removals]
  (clj->js (apply dissoc base removals)))

(defn fit-in-range [n mi ma] (max (min n ma) mi))

(defn target-value [e] (.. e -target -value))

;; svg helpers

(defn svg-line [x1 y1 x2 y2]
  (dom/line #js {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :style #js {:stroke "#000" :strokeWidth "2" :strokeLinecap "round"}}))

;; components

(defn comp-option [{:keys [label value]}] (dom/option #js {:value value} label))

(defn comp-select [{:keys [options] :as attributes}]
  (apply dom/select (attrs attributes :options)
         (map comp-option options)))

(defn comp-grid-backgrounds [{:keys [width color-fn]
                         :or {color-fn color-compute-blue-to-red}}
                        {:keys [marks columns]}]
  (let [max-distance (get marks (farthest-point marks))
        cell-size (/ width columns)
        mark->rect (fn [[cell distance]]
                     (let [[x y] (cell-bounds cell cell-size)]
                       (dom/rect #js {:width cell-size :height cell-size :x x :y y
                                      :style #js {:fill (color-fn (/ distance max-distance))}})))]
    (apply dom/g nil (map mark->rect marks))))

(defn comp-grid-lines [{:keys [width]} {:keys [columns] :as grid}]
  (let [cell-size (/ width columns)
        link->line (fn [cell]
                     (let [[x1 y1 x2 y2] (cell-bounds cell cell-size)
                           lines (->> [(if-not (valid-pos? grid (north cell)) [x1 y1 x2 y1])
                                       (if-not (valid-pos? grid (west cell)) [x1 y1 x1 y2])
                                       (if-not (linked-to? grid cell (east cell)) [x2 y1 x2 y2])
                                       (if-not (linked-to? grid cell (south cell)) [x1 y2 x2 y2])]

                                      (filter identity))]
                       (apply dom/g #js {:key (pr-str cell)} (map #(apply svg-line %) lines))))]
    (apply dom/g nil (map link->line (cells-seq grid)))))

(defn comp-grid-dead-ends [{:keys [width]} {:keys [dead-ends columns]}]
  (let [cell-size (/ width columns)
        mark->rect (fn [cell]
                     (let [[x y] (cell-bounds cell cell-size)]
                       (dom/rect #js {:width cell-size :height cell-size :x x :y y
                                      :style #js {:fill "rgba(255, 255, 0, 0.3)"}})))]
    (apply dom/g nil (map mark->rect (keys dead-ends)))))

(defn comp-layer-toggler [layer {:keys [data bus]}]
  (dom/input #js {:type "checkbox" :checked (get-in data [:layers layer :show])
                  :onChange #(put! bus [:update-layer layer :show (.. % -target -checked)])}))

(defn maze-playground [{:keys [generator grid-size] :as data} owner]
  (reify
    om/IDisplayName (display-name [_] "Maze Playground")
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
            (om/update! data :grid-size (fit-in-range n 1 100))))

        (go-sub pub :update-layer [_ layer prop value]
          (om/transact! data #(assoc-in % [:layers layer prop] value)))

        (go-sub pub :generate-maze [_]
          (try
            (let [grid-size (:grid-size @app-state)
                  _ (assert (> grid-size 1) "Grid size must be bigger than 1")
                  generator (get-in opt-algorithms [(:generator @app-state) :value])
                  mask (m/ascii-mask "X........X"
                                     "....XX...."
                                     "...XXXX..."
                                     "....XX...."
                                     "X........X"
                                     "X........X"
                                     "....XX...."
                                     "...XXXX..."
                                     "....XX...."
                                     "X........X")
                  maze (bench "generating maze" (-> (m/make-grid grid-size grid-size)
                                                    (update :mask into mask)
                                                    generator))
                  marks (bench "generating marks" (-> (m/dijkstra-enumerate maze (m/rand-cell maze))))]
              (om/update! data :maze (assoc maze :marks marks :dead-ends (bench "dead ends" (m/dead-ends maze)))))
            (catch js/Error e
              (print "Error generating maze: " (.-message e)))))))

    om/IRender
    (render [_]
      (let [bus (om/get-state owner :bus)
            flux {:data data :bus bus}
            color-fn (get-in data [:layers :distance-mash :color-fn])]
        (dom/div nil
          (dom/label #js {:style #js {:display "block"}}
            "Generator algorithm: "
            (comp-select {:value    (name generator) :options (impl->options opt-algorithms)
                          :onChange #(put! bus [:update-generator (target-value %)])}))
          (dom/label #js {:style #js {:display "block"}}
            "Grid size: "
            (dom/input #js {:type     "number" :value grid-size :min 1 :max 100
                            :onChange #(put! bus [:update-grid-size (target-value %)])}))
          (dom/button #js {:onClick #(put! bus [:generate-maze])
                           :style #js {:margin-top "10px"}} "Generate maze")

          (dom/div nil
            "Distance Gradient layer: "
            (comp-layer-toggler :distance-mash flux)
            (comp-select {:value    (name color-fn) :options (impl->options opt-color-functions)
                          :onChange #(put! bus [:update-layer :distance-mash :color-fn (keyword (target-value %))])}))
          (dom/div nil
            "Dead Ends layer: "
            (comp-layer-toggler :dead-ends flux))
          (dom/div nil
            "Grid Lines layer: "
            (comp-layer-toggler :grid-lines flux))

          (dom/hr nil)

          (let [size {:width 600 :height 600}]
            (dom/svg (clj->js size)
              (if (get-in data [:layers :distance-mash :show])
                (comp-grid-backgrounds (assoc size :color-fn (get-in opt-color-functions [color-fn :value])) (:maze data)))
              (if (get-in data [:layers :dead-ends :show]) (comp-grid-dead-ends size (:maze data)))
              (if (get-in data [:layers :grid-lines :show]) (comp-grid-lines size (:maze data))))))))))

;; initializer

(defn build-at [node]
  (let [root (om/root maze-playground app-state {:target node})]
    (om/get-state root)))
