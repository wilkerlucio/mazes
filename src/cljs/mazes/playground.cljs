(ns mazes.playground
  (:require-macros [wilkerdev.util.macros :refer [bench go-sub go-sub*]]
                   [cljs.core.async.macros :refer [go]])
  (:require [mazes.core :refer [cells-seq valid-pos? linked-to? north east south west
                                make-grid rand-cell dijkstra-enumerate farthest-point]
             :as m]
            [clojure.string :as str]
            [om.core :as om]
            [om.dom :as dom]
            [cljs.core.async :refer [chan put! <!] :as async]
            [wilkerdev.util.dom :as domm])
  (:import [goog.fs FileReader]))

;; state and data

(def initial-state
  {:grid-size      {:columns 10 :rows 10}
   :generator      :recursive-backtracker
   :marker-builder :random-point
   :colorizer      :blue-to-red
   :mask           #{}
   :layers         {:distance-mash {:show true
                                    :color-fn :blue-to-red}
                    :dead-ends     {:show false}
                    :grid-lines    {:show true}}})

(def opt-algorithms
  (sorted-map
    :none {:label "None"
           :value identity}
    :aldous-broder {:label "Aldous Broder"
                    :value m/gen-aldous-broder}
    :binary-tree {:label "Binary Tree"
                  :value m/gen-binary-tree
                  :disable? #(apply not= (vals (:grid-size %)))}
    :hunt-and-kill {:label "Hunt and Kill"
                    :value m/gen-hunt-and-kill}
    :recursive-backtracker {:label "Recursive Backtracker"
                            :value m/gen-recursive-backtracker}
    :sidewinder {:label "Sidewinder"
                 :value m/gen-sidewinder
                 :disable? #(apply not= (vals (:grid-size %)))}
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

;; serialization

(defprotocol ISerialize
  (serialize-type-key [this]))

(defn serialize-record [grid]
  (-> (into {} grid)
      (assoc :grid-type (serialize-type-key grid))))

(defmulti unserialize-record* :grid-type)

(defn unserialize-record [grid]
  (-> (unserialize-record* grid)
      (dissoc :grid-type)))

(extend-protocol ISerialize
  m/RectangularGrid
  (serialize-type-key [_] ::rectangular)

  m/PolarGrid
  (serialize-type-key [_] ::polar))

(defmethod unserialize-record* ::rectangular [attrs] (make-grid attrs))
(defmethod unserialize-record* ::polar [attrs] (merge (m/make-polar-grid 1) attrs))

;; helpers

(defn rect-center [{:keys [width height]}] [(/ width 2) (/ height 2)])

(defn cell-bounds [[y x] cell-size]
  [(* x cell-size) (* y cell-size)
   (* (inc x) cell-size) (* (inc y) cell-size)])

(defn impl->options [m data]
  (map (fn [[k v]] {:label     (:label v) :value (name k)
                    :disabled? ((get v :disable? (fn [_] false)) data)}) m))

(defn attrs [base & removals]
  (clj->js (apply dissoc base removals)))

(defn fit-in-range [n mi ma] (max (min n ma) mi))

(defn target-value [e] (.. e -target -value))

(defn load-image [data]
  (let [c (chan)]
    (doto (domm/create-element "img")
      (aset "onload" #(put! c (.-target %)))
      (aset "src" data))
    c))

(defn create-canvas [width height]
  (doto (domm/create-element "canvas")
    (aset "width" width)
    (aset "height" height)))

(defn png-mask [png-data]
  (go
    (let [img (<! (load-image png-data))
          width (.-width img) height (.-height img)
          canvas (create-canvas width height)
          ctx (.getContext canvas "2d")]
      (.drawImage ctx img 0 0)
      (let [mask (->> (.getImageData ctx 0 0 width height)
                      (.-data)
                      (array-seq)
                      (partition 4)                         ; [[r g b a] [r g b a] ...]
                      (map vector (range))                  ; [[0 [r g b a] [1 [r g b a]] ...]
                      (filter (fn [[_ [r g b]]] (= 0 r g b))) ; keep blacks
                      (map (fn [[pos]] [(int (/ pos width)) (mod pos width)]))
                      (set))]
        {:rows height :columns width :mask mask}))))

(defn- read-files [e]
  (->> e
       .-dataTransfer
       .-files
       array-seq))

(defn read-file-as-data-url
  ([file] (read-file-as-data-url file (chan)))
  ([file c]
   (doto (.readAsDataUrl FileReader file)
     (.then #(put! c %)))
   c))

(defn prevent-global-drop! []
  (set! (.-ondragover js/window) #(.preventDefault %))
  (set! (.-ondrop js/window) #(.preventDefault %)))

(defn compute-cell-size [{:keys [width height]} {:keys [columns rows]}]
  (min (/ width columns)
       (/ height rows)))

(defn polar-coordinates [{:keys [rows] {:keys [height] :as size} ::dimensions} [y x]]
  (let [ring-height (-> (/ height rows 2))
        theta (-> (* 2 Math/PI) (/ (m/polar-count-row-cells rows y)))]
    {:inner-radius (* y ring-height)
     :outer-radius (* (inc y) ring-height)
     :theta-ccw    (* x theta)
     :theta-cw     (* (inc x) theta)
     :center       (rect-center size)}))

(defn polar->cartesian [{:keys [inner-radius outer-radius theta-ccw theta-cw] [x y] :center}]
  [(-> (* inner-radius (Math/cos theta-ccw)) (+ x))
   (-> (* inner-radius (Math/sin theta-ccw)) (+ y))
   (-> (* inner-radius (Math/cos theta-cw)) (+ x))
   (-> (* inner-radius (Math/sin theta-cw)) (+ y))
   (-> (* outer-radius (Math/cos theta-cw)) (+ x))
   (-> (* outer-radius (Math/sin theta-cw)) (+ y))
   (-> (* outer-radius (Math/cos theta-ccw)) (+ x))
   (-> (* outer-radius (Math/sin theta-ccw)) (+ y))])

;; svg helpers

(defn svg-line [x1 y1 x2 y2 style]
  (dom/line #js {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :style (clj->js style)}))

;; components

(defprotocol IRenderGrid
  (draw-cell [grid cell style])
  (draw-grid-edges [grid style]))

(extend-protocol IRenderGrid
  m/RectangularGrid
  (draw-cell [{:keys [::dimensions] :as grid} cell style]
    (let [cell-size (compute-cell-size dimensions grid)
          [x y] (cell-bounds cell cell-size)]
      (dom/rect #js {:width cell-size :height cell-size :x x :y y
                     :style (clj->js style)})))

  (draw-grid-edges [{:keys [::dimensions] :as grid} style]
    (let [cell-size (compute-cell-size dimensions grid)
          link->line (fn [cell]
                       (let [[x1 y1 x2 y2] (cell-bounds cell cell-size)
                             lines (->> [(if-not (valid-pos? grid (north cell)) [x1 y1 x2 y1])
                                         (if-not (valid-pos? grid (west cell)) [x1 y1 x1 y2])
                                         (if-not (linked-to? grid cell (east cell)) [x2 y1 x2 y2])
                                         (if-not (linked-to? grid cell (south cell)) [x1 y2 x2 y2])]

                                        (filter identity))]
                         (apply dom/g #js {:key (pr-str cell)} (map #(apply svg-line (conj % style)) lines))))]
      (apply dom/g nil (map link->line (cells-seq grid)))))

  m/PolarGrid
  (draw-cell [grid cell style]
    (if (= cell [0 0])
      (let [{:keys [rows] {:keys [height]} ::dimensions} grid
            [cx cy] (rect-center (::dimensions grid))
            radius (/ height rows 2)]
        (dom/circle #js {:cx cx :cy cy :r radius :style (clj->js style)}))
      (let [{:keys [inner-radius outer-radius] :as coords} (polar-coordinates grid cell)
            [ax ay bx by cx cy dx dy] (polar->cartesian coords)]
        (dom/path #js {:d (str "M" ax "," ay " "
                               "A" inner-radius "," inner-radius " 0 0,1 " bx "," by " "
                               "L" cx "," cy " "
                               "A" outer-radius "," outer-radius " 0 0,0 " dx "," dy)
                       :style (clj->js style)}) )))

  (draw-grid-edges [{:keys [rows] :as grid {:keys [width height]} ::dimensions} style]
    (let [style (clj->js style)
          ring-height (-> (/ height rows 2))
          [x y] [(/ width 2) (/ height 2)]
          lines (for [cell (m/cells-seq grid)
                      :when (> (get cell 0) 0)
                      :let [{:keys [inner-radius] :as coords} (polar-coordinates grid cell)
                            [ax ay bx by cx cy] (polar->cartesian coords)]]
                  (dom/g nil
                    (if-not (m/linked-to? grid cell (m/polar-cell-inward grid cell))
                      (dom/path #js {:d (str "M" ax "," ay " "
                                             "A" inner-radius "," inner-radius " 0 0,1 " bx "," by)
                                     :style style}))
                    (if-not (m/linked-to? grid cell (m/polar-cell-cw grid cell)) (svg-line bx by cx cy style))))]
      (apply dom/g nil (dom/circle #js {:cx x :cy y :r (* rows ring-height) :style style}) lines))))

(defn comp-option [{:keys [label value disabled?]}] (dom/option #js {:value value :disabled disabled?} label))

(defn comp-select [{:keys [options] :as attributes}]
  (apply dom/select (attrs attributes :options)
         (map comp-option options)))

(defn comp-grid-backgrounds [{:keys [marks color-fn] :as grid}]
  (let [max-distance (get marks (farthest-point marks))
        mark->cell (fn [[cell distance]]
                     (draw-cell grid cell {:fill (color-fn (/ distance max-distance))}))]
    (apply dom/g nil (map mark->cell marks))))

(defn comp-grid-dead-ends [{:keys [dead-ends] :as grid}]
  (let [mark->cell (fn [cell] (draw-cell grid cell {:fill "rgba(255, 255, 0, 0.3)"}))]
    (apply dom/g nil (map mark->cell (keys dead-ends)))))

(defn comp-layer-toggler [layer {:keys [data bus]}]
  (dom/input #js {:type "checkbox" :checked (get-in data [:layers layer :show])
                  :onChange #(put! bus [:update-layer layer :show (.. % -target -checked)])}))

(defn file-dropper [[{:keys [onDrop] :as opts} view] _]
  (reify
    om/IRender
    (render [_]
      (let [attrs (assoc opts :onDrop (fn [e] (.preventDefault e) (onDrop (read-files e)))
                              :onDragEnter #(.preventDefault %)
                              :onDragOver  #(.preventDefault %)
                              :onDragLeave #(.preventDefault %)
                              :onDragEnd   #(.preventDefault %))]
        (dom/div (clj->js attrs)
          view)))))

(defn maze-services [data owner]
  (let [pub (om/get-state owner :pub)
        bus (om/get-state owner :bus)]

    (go-sub pub :update-generator [_ generator]
      (om/update! data :generator (keyword generator)))

    (go-sub pub :update-grid-size [_ axis size]
      (let [n (or (js/parseInt size) 0)]
        (om/update! (om/get-props owner) [:grid-size axis] (fit-in-range n 1 100))))

    (go-sub pub :update-layer [_ layer prop value]
      (om/transact! data #(assoc-in % [:layers layer prop] value)))

    (go-sub pub :mask-dropped [_ file]
      (let [{:keys [rows columns mask]} (-> (read-file-as-data-url file) <!
                                            (png-mask) <!)]
        (om/transact! data (fn [d]
                             (-> (assoc d :mask mask)
                                 (assoc-in [:grid-size :rows] rows)
                                 (assoc-in [:grid-size :columns] columns))))
        (<! (async/timeout 1))
        (put! bus [:generate-maze])))

    (go-sub pub :generate-maze [_]
      (try
        (let [cur-data (om/get-props owner)
              {:keys [columns rows] :as grid-size} (:grid-size cur-data)
              _ (assert (some #(> % 1) (vals grid-size)) "Grid size must be bigger than 1")
              generator (get-in opt-algorithms [(:generator cur-data) :value])
              grid (bench "generating maze" (-> (m/make-polar-grid rows)
                                                #_ (m/make-grid rows columns)
                                                (update :mask into (:mask cur-data))
                                                generator))
              ;marks (bench "generating marks" (-> (m/dijkstra-enumerate grid (m/rand-cell grid))))
              marks (bench "generating marks" (-> (m/dijkstra-enumerate grid [0 0])))
              ;marks (bench "generating marks" (-> (m/longest-path-marks grid)))
              dead-ends (bench "dead ends" (m/dead-ends grid))]
          (om/update! data :grid (-> (assoc grid :marks marks :dead-ends dead-ends)
                                     (serialize-record))))
        (catch js/Error e
          (print "Error generating maze: " (.-message e)))))))

(defn maze-playground [{:keys [generator grid-size] :as data} owner]
  (reify
    om/IDisplayName (display-name [_] "Maze Playground")
    om/IInitState
    (init-state [_]
      (let [bus (chan 1024)
            pub (async/pub bus first)]
        {:bus bus :pub pub}))
    om/IWillMount
    (will-mount [_] (maze-services data owner))

    om/IRender
    (render [_]
      (let [bus (om/get-state owner :bus)
            flux {:data data :bus bus}
            color-fn (get-in data [:layers :distance-mash :color-fn])]
        (dom/div nil
          (dom/label #js {:style #js {:display "block"}}
            "Generator algorithm: "
            (comp-select {:value    (name generator) :options (impl->options opt-algorithms data)
                          :onChange #(put! bus [:update-generator (target-value %)])}))
          (dom/label #js {:style #js {:display "block"}}
            "Grid size: "
            (dom/input #js {:type     "number" :value (:columns grid-size) :min 1 :max 100
                            :onChange #(put! bus [:update-grid-size :columns (target-value %)])})
            (dom/input #js {:type     "number" :value (:rows grid-size) :min 1 :max 100
                            :onChange #(put! bus [:update-grid-size :rows (target-value %)])}))
          (dom/button #js {:onClick #(put! bus [:generate-maze])
                           :style #js {:marginTop "10px"}} "Generate maze")

          (dom/div nil
            "Distance Gradient layer: "
            (comp-layer-toggler :distance-mash flux)
            (comp-select {:value    (name color-fn) :options (impl->options opt-color-functions data)
                          :onChange #(put! bus [:update-layer :distance-mash :color-fn (keyword (target-value %))])}))
          (dom/div nil
            "Dead Ends layer: "
            (comp-layer-toggler :dead-ends flux))
          (dom/div nil
            "Grid Lines layer: "
            (comp-layer-toggler :grid-lines flux))

          (dom/hr nil)

          (if-let [grid (:grid data)]
            (let [size {:width 600 :height 600}
                  grid (-> (unserialize-record grid)
                           (assoc ::dimensions size))]
              (om/build file-dropper [{:onDrop #(put! bus [:mask-dropped (first %)])}
                                      (dom/svg (clj->js size)
                                        (if (get-in data [:layers :distance-mash :show])
                                          (comp-grid-backgrounds (assoc grid :color-fn (get-in opt-color-functions [color-fn :value]))))
                                        (if (get-in data [:layers :dead-ends :show]) (comp-grid-dead-ends grid))
                                        (if (get-in data [:layers :grid-lines :show]) (draw-grid-edges grid {:stroke "#000" :fill "none" :strokeWidth 2})))]))))))))

;; initializer

(defn build-at [node app-state]
  (prevent-global-drop!)
  (let [root (om/root maze-playground app-state {:target node})]
    (om/get-state root)))
