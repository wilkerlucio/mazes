(ns ^:figwheel-always mazes.playground
  (:require-macros [wilkerdev.util.macros :refer [bench go-sub go-sub*]]
                   [cljs.core.async.macros :refer [go]])
  (:require [mazes.core :refer [cells-seq valid-pos? linked-to? north east south west
                                make-grid rand-cell dijkstra-enumerate farthest-point]
             :as m]
            [om.core :as om]
            [om.dom :as dom]
            [cljs.core.async :refer [chan put! <!] :as async]
            [om-bootstrap.button :as bs-b]
            [om-bootstrap.input :as bs-i]
            [wilkerdev.util :refer [distinct-consecutive]]
            [wilkerdev.util.dom :as domm])
  (:import [goog.fs FileReader]))

;; state and data

(def initial-state
  {:grid-type      :polar
   :grid-size      {:columns 10 :rows 10}
   :generator      :recursive-backtracker
   :marker-builder :random-point
   :colorizer      :blue-to-red
   :mask           #{}
   :layers         {:distance-mash {:show true
                                    :color-fn :blue-to-red}
                    :dead-ends     {:show false}
                    :path          {:show true}
                    :grid-lines    {:show true}}})

(def opt-grid-types
  (sorted-map
    :rectangular {:label "Rectangular Grid"
                  :value (fn [{{:keys [columns rows]} :grid-size}] (m/make-grid columns rows))}
    :polar {:label "Polar Grid"
            :value (fn [{{:keys [rows]} :grid-size}] (m/make-polar-grid rows))}
    :hex {:label "Hexagon Grid"
          :value (fn [{{:keys [columns rows]} :grid-size}] (m/make-hex-grid columns rows))}
    :triangle {:label "Triangle Grid"
               :value (fn [{{:keys [columns rows]} :grid-size}] (m/make-triangle-grid columns rows))}))

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
  (serialize-type-key [_] ::polar)

  m/HexGrid
  (serialize-type-key [_] ::hexagon)

  m/TriangleGrid
  (serialize-type-key [_] ::triangle))

(defmethod unserialize-record* ::rectangular [attrs] (make-grid attrs))
(defmethod unserialize-record* ::polar [attrs] (merge (m/make-polar-grid nil) attrs))
(defmethod unserialize-record* ::hexagon [attrs] (merge (m/make-hex-grid nil nil) attrs))
(defmethod unserialize-record* ::triangle [attrs] (merge (m/make-triangle-grid nil nil) attrs))

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
          ctx (.getContext canvas "2d")
          get-y #(int (/ % width))
          get-x #(mod % width)
          pos->coord (juxt get-y get-x)]
      (.drawImage ctx img 0 0)
      (let [mask (->> (.getImageData ctx 0 0 width height)
                      (.-data)
                      (array-seq)
                      (partition 4)                         ; [[r g b a] [r g b a] ...]
                      (map vector (range))                  ; [[0 [r g b a] [1 [r g b a]] ...]
                      (filter (fn [[_ [r g b]]] (= 0 r g b))) ; keep blacks
                      (map first)                           ; keep position, discard colors
                      (map pos->coord)
                      (set))]
        {:rows height :columns width :mask mask}))))

(defn read-files [e]
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

;; rectangular grid helpers

(defn rect-cell-size [{:keys [width height columns rows]}]
  (min (/ width columns) (/ height rows)))

;; polar grid helpers

(defn polar-coordinates [{:keys [rows height] :as grid} [y x]]
  (let [ring-height (-> (/ height rows 2))
        theta (-> (* 2 Math/PI) (/ (m/polar-count-row-cells rows y)))]
    {:inner-radius (* y ring-height)
     :outer-radius (* (inc y) ring-height)
     :theta-ccw    (* x theta)
     :theta-cw     (* (inc x) theta)
     :center       (rect-center grid)}))

(defn polar->cartesian [{:keys [inner-radius outer-radius theta-ccw theta-cw] [x y] :center}]
  [(-> (* inner-radius (Math/cos theta-ccw)) (+ x))
   (-> (* inner-radius (Math/sin theta-ccw)) (+ y))
   (-> (* inner-radius (Math/cos theta-cw)) (+ x))
   (-> (* inner-radius (Math/sin theta-cw)) (+ y))
   (-> (* outer-radius (Math/cos theta-cw)) (+ x))
   (-> (* outer-radius (Math/sin theta-cw)) (+ y))
   (-> (* outer-radius (Math/cos theta-ccw)) (+ x))
   (-> (* outer-radius (Math/sin theta-ccw)) (+ y))])

;; hex grid helpers

(defn hex-measures [{:keys [width columns]} [y x]]
  (let [a (/ width columns 4)
        s (* a 2)
        b (-> (.sqrt js/Math 3) (* s) (/ 2))
        cw (* s 2)
        ch (* b 2)
        cx (-> (* 3 a x) (+ s))
        cy (-> (* y ch) (+ (if (odd? x) (* 2 b) b)))
        ; f/n = far/near
        ; n/s/e/w = north/south/east/west
        x-fw (- cx s) x-nw (- cx a)
        x-fe (+ cx s) x-ne (+ cx a)
        y-n (- cy b) y-s (+ cy b)]
    {:s     s :a a :b b :cw cw :ch ch :cx cx :cy cy
     :x-fw  x-fw :x-nw x-nw :x-ne x-ne :x-fe x-fe :y-n y-n :y-s y-s
     ;   1  2
     ; 6      3
     ;   5  4
     :edges [x-nw y-n
             x-ne y-n
             x-fe cy
             x-ne y-s
             x-nw y-s
             x-fw cy]}))

;; triangle grid helpers

(defn triangle-measures [{:keys [width columns]} [y x :as cell]]
  (let [s (* 1.8 (/ width columns))
        cw s
        chw (/ cw 2)
        ch (-> (* s (.sqrt js/Math 3)) (/ 2))
        chh (/ ch 2)
        cx (+ chw (* x chw))
        cy (+ chh (* y ch))
        wx (- cx chw)
        mx cx
        ex (+ cx chw)
        [ay by] (if (m/cell-upright? cell)
                  [(- cy chh) (+ cy chh)]
                  [(+ cy chh) (- cy chh)])]
    {:wx wx :mx mx :ex ex :ay ay :by by :edges [wx by mx ay ex by]}))

;; svg helpers

(defn svg-line [x1 y1 x2 y2 style]
  (dom/line #js {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :style (clj->js style)}))

;; services

(defn maze-services [data owner]
  (let [pub (om/get-state owner :pub)
        bus (om/get-state owner :bus)]

    (go-sub pub :update-grid-type [_ grid-type]
      (om/update! data :grid-type (keyword grid-type)))

    (go-sub pub :update-generator [_ generator]
      (om/update! data :generator (keyword generator)))

    (go-sub pub :update-grid-size [_ axis size]
      (let [n (or (js/parseInt size) 0)]
        (om/update! data [:grid-size axis] (fit-in-range n 1 100))))

    (go-sub pub :update-layer [_ layer prop value]
      (om/transact! data #(assoc-in % [:layers layer prop] value)))

    (go-sub* pub :select-cell [_ cell] (chan 1 (distinct-consecutive))
      (let [{{:keys [marks] :as grid} :grid} (om/get-props owner)
            route (m/trace-route-back (unserialize-record grid) marks cell)]
        (om/update! data :render-path route)
        (put! bus [:update-layer :path :show true])))

    (go-sub pub :mask-dropped [_ file]
      (if (= :rectangular (get (om/get-props owner) :grid-type))
        (let [{:keys [rows columns mask]} (-> (read-file-as-data-url file) <!
                                              (png-mask) <!)]
          (om/transact! data (fn [d]
                               (-> (assoc d :mask mask)
                                   (assoc-in [:grid-size :rows] rows)
                                   (assoc-in [:grid-size :columns] columns))))
          (<! (async/timeout 1))
          (put! bus [:generate-maze]))))

    (go-sub pub :generate-maze [_]
      (try
        (let [cur-data (om/get-props owner)
              grid-size (:grid-size cur-data)
              _ (assert (some #(> % 1) (vals grid-size)) "Grid size must be bigger than 1")
              generator (get-in opt-algorithms [(:generator cur-data) :value])
              grid-builder (get-in opt-grid-types [(:grid-type cur-data) :value])
              grid (bench "generating maze" (-> (grid-builder cur-data)
                                                #_ (m/make-grid rows columns)
                                                (update :mask into (:mask cur-data))
                                                generator))
              marks (bench "generating marks" (-> (m/dijkstra-enumerate grid (m/rand-cell grid))))
              ;marks (bench "generating marks" (-> (m/dijkstra-enumerate grid [0 0])))
              ;marks (bench "generating marks" (-> (m/longest-path-marks grid)))
              dead-ends (bench "dead ends" (m/dead-ends grid))]
          (om/update! data :grid (-> (assoc grid :marks marks :dead-ends dead-ends)
                                     (serialize-record)))
          (om/update! data :render-path []))
        (catch js/Error e
          (print "Error generating maze: " (.-message e)))))))

;; components

(defprotocol IRenderGrid
  (draw-cell [grid cell style])
  (draw-grid-edges [grid style]))

(extend-type m/RectangularGrid
  IRenderGrid
  (draw-cell [grid cell attributes]
    (let [cell-size (rect-cell-size grid)
          [x y] (cell-bounds cell cell-size)]
      (dom/rect (clj->js (merge {:width cell-size :height cell-size :x x :y y}
                                attributes)))))

  (draw-grid-edges [grid style]
    (let [cell-size (rect-cell-size grid)
          link->line (fn [cell]
                       (let [[x1 y1 x2 y2] (cell-bounds cell cell-size)
                             lines (->> [(if-not (valid-pos? grid (north cell)) [x1 y1 x2 y1])
                                         (if-not (valid-pos? grid (west cell)) [x1 y1 x1 y2])
                                         (if-not (linked-to? grid cell (east cell)) [x2 y1 x2 y2])
                                         (if-not (linked-to? grid cell (south cell)) [x1 y2 x2 y2])]

                                        (filter identity))]
                         (apply dom/g #js {:key (pr-str cell)} (map #(apply svg-line (conj % style)) lines))))]
      (apply dom/g nil (map link->line (cells-seq grid))))))

(extend-type m/PolarGrid
  IRenderGrid
  (draw-cell [grid cell attributes]
    (if (= cell [0 0])
      (let [{:keys [rows height]} grid
            [cx cy] (rect-center grid)
            radius (/ height rows 2)]
        (dom/circle (clj->js (merge {:cx cx :cy cy :r radius} attributes))))
      (let [{:keys [inner-radius outer-radius] :as coords} (polar-coordinates grid cell)
            [ax ay bx by cx cy dx dy] (polar->cartesian coords)]
        (dom/path (clj->js (merge {:d       (str "M" ax "," ay " "
                                                 "A" inner-radius "," inner-radius " 0 0,1 " bx "," by " "
                                                 "L" cx "," cy " "
                                                 "A" outer-radius "," outer-radius " 0 0,0 " dx "," dy)}
                                  attributes))))))

  (draw-grid-edges [{:keys [rows height] :as grid} style]
    (let [style (clj->js style)
          ring-height (-> (/ height rows 2))
          [x y] (rect-center grid)
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

(extend-type m/HexGrid
  IRenderGrid
  (draw-cell [grid cell attributes]
    (let [{[mx my & l] :edges} (hex-measures grid cell)]
      (dom/path (clj->js (merge {:d (apply str "M" mx "," my " " (map (fn [[x y]] (str "L" x "," y " ")) (partition 2 l)))}
                                attributes)))))
  (draw-grid-edges [grid style]
    (let [link->line (fn [cell]
                       (let [{:keys [cy x-fw x-nw x-ne x-fe y-n y-s]} (hex-measures grid cell)
                             lines (->> [(if-not (valid-pos? grid (m/southwest cell)) [x-fw cy x-nw y-s])
                                         (if-not (valid-pos? grid (m/northwest cell)) [x-fw cy x-nw y-n])
                                         (if-not (valid-pos? grid (m/north cell)) [x-nw y-n x-ne y-n])
                                         (if-not (linked-to? grid cell (m/northeast cell)) [x-ne y-n x-fe cy])
                                         (if-not (linked-to? grid cell (m/southeast cell)) [x-ne y-s x-fe cy])
                                         (if-not (linked-to? grid cell (m/south cell)) [x-nw y-s x-ne y-s])]

                                        (filter identity))]
                         (apply dom/g #js {:key (pr-str cell)} (map #(apply svg-line (conj % style)) lines))))]
      (apply dom/g nil (map link->line (cells-seq grid))))))

(extend-type m/TriangleGrid
  IRenderGrid
  (draw-cell [grid cell attributes]
    (let [{[mx my & l] :edges} (triangle-measures grid cell)]
      (dom/path (clj->js (merge {:d (apply str "M" mx "," my " " (map (fn [[x y]] (str "L" x "," y " ")) (partition 2 l)))}
                                attributes)))))
  (draw-grid-edges [grid style]
    (let [link->line (fn [cell]
                       (let [{:keys [wx mx ex ay by]} (triangle-measures grid cell)
                             no-south (and (m/cell-upright? cell) (false? (valid-pos? grid (m/south cell))))
                             not-linked (and (not (m/cell-upright? cell)) (not (linked-to? grid cell (m/north cell))))
                             lines (->> [(if-not (valid-pos? grid (m/west cell)) [wx by mx ay])
                                         (if-not (linked-to? grid cell (m/east cell)) [ex by mx ay])
                                         (if (or no-south not-linked) [ex by wx by])]

                                        (filter identity))]
                         (apply dom/g #js {:key (pr-str cell)} (map #(apply svg-line (conj % style)) lines))))]
      (apply dom/g nil (map link->line (cells-seq grid))))))

(defn comp-option [{:keys [label value disabled?]}] (dom/option #js {:value value :disabled disabled?} label))

(defn comp-select [{:keys [options] :as attributes}]
  (apply dom/select (attrs attributes :options)
         (map comp-option options)))

(defn comp-grid-backgrounds [{:keys [marks color-fn] :as grid} bus]
  (let [max-distance (get marks (farthest-point marks))
        mark->cell (fn [[cell distance]]
                     (draw-cell grid cell {:fill (color-fn (/ distance max-distance))
                                           :onClick #(put! bus [:select-cell cell])}))]
    (apply dom/g nil (map mark->cell marks))))

(defn comp-grid-dead-ends [{:keys [dead-ends] :as grid}]
  (let [mark->cell (fn [cell] (draw-cell grid cell {:fill "rgba(255, 255, 0, 0.3)"}))]
    (apply dom/g #js {:style #js {:pointerEvents "none"}} (map mark->cell (keys dead-ends)))))

(defn comp-layer-toggler [layer {:keys [data bus]}]
  (dom/input #js {:type "checkbox" :checked (get-in data [:layers layer :show])
                  :onChange #(put! bus [:update-layer layer :show (.. % -target -checked)])}))

(defn comp-grid-path [grid path]
  (let [mark->cell (fn [cell] (draw-cell grid cell {:fill "rgba(0, 255, 0, 0.3)"}))]
    (apply dom/g #js {:style #js {:pointerEvents "none"}} (map mark->cell path))))

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
        (dom/div #js {:className "flex-row flex"}
          (dom/div #js {:className "playground-menu"}
            (dom/div #js {:className "playground-menu-header"} "Grid Type")
            (dom/div #js {:className "playground-menu-content"}
              (comp-select {:value    (name (:grid-type data)) :options (impl->options opt-grid-types data)
                            :onChange #(put! bus [:update-grid-type (target-value %)])}))

            (dom/div #js {:className "playground-menu-header"} "Generator Algorithm")
            (dom/div #js {:className "playground-menu-content"}
              (comp-select {:value    (name generator) :options (impl->options opt-algorithms data)
                            :onChange #(put! bus [:update-generator (target-value %)])}))

            (dom/div #js {:className "playground-menu-header"} "Grid Size")
            (dom/div #js {:className "playground-menu-content"}
              (if-not (= :polar (:grid-type data))
                (bs-i/input {:type       "number" :value (:columns grid-size) :min 1 :max 100
                             :on-change  #(put! bus [:update-grid-size :columns (target-value %)])
                             :standalone true}))
              (bs-i/input {:type       "number" :value (:rows grid-size) :min 1 :max 100
                           :on-change  #(put! bus [:update-grid-size :rows (target-value %)])
                           :standalone true}))


            (dom/div #js {:className "playground-menu-header"} "Layers")
            (dom/div #js {:className "playground-menu-content"}
              (dom/div nil
                "Distance Gradient layer: "
                (comp-layer-toggler :distance-mash flux)
                (comp-select {:value    (name color-fn) :options (impl->options opt-color-functions data)
                              :onChange #(put! bus [:update-layer :distance-mash :color-fn (keyword (target-value %))])}))
              (dom/div nil
                "Dead Ends layer: "
                (comp-layer-toggler :dead-ends flux))
              (dom/div nil
                "Path resolution layer: "
                (comp-layer-toggler :path flux))
              (dom/div nil
                "Grid Lines layer: "
                (comp-layer-toggler :grid-lines flux)))

            (dom/div #js {:className "text-center"}
              (bs-b/button {:on-click #(put! bus [:generate-maze])
                            :bs-style "success" :bs-size "large"} "Generate maze")))

          (dom/div #js {:className "playground-content flex"}
            (if-let [grid (:grid data)]
              (let [size {:width 600 :height 600}
                    grid (-> (unserialize-record grid)
                             (merge size))
                    grid-svg
                    (dom/svg (clj->js (assoc size
                                        :className "playground-svg"
                                        :viewBox "-5 -5 610 610"))
                             (let [color-fn (if (get-in data [:layers :distance-mash :show])
                                              (get-in opt-color-functions [color-fn :value])
                                              (fn [_] "transparent"))]
                               (comp-grid-backgrounds (assoc grid :color-fn color-fn) bus))
                             (if (get-in data [:layers :dead-ends :show]) (comp-grid-dead-ends grid))
                             (if (get-in data [:layers :path :show]) (comp-grid-path grid (get data :render-path [])))
                             (if (get-in data [:layers :grid-lines :show]) (draw-grid-edges grid {:stroke "#000" :fill "none" :strokeWidth 2})))]
                grid-svg))))))))

;; initializer

(defn build-at [node app-state]
  (prevent-global-drop!)
  (let [root (om/root maze-playground app-state {:target node})]
    (om/get-state root)))
