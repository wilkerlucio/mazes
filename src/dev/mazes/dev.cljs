(ns mazes.dev
  (:require [figwheel.client :as fw]
            [mazes.core]
            [mazes.playground]
            [mazes.core-test]
            [cemerick.cljs.test :as t]))

(defn refresh []
  (mazes.playground/build-at (.getElementById js/document "app-container"))
  #_ (mazes.playground/draw-maze (.querySelector js/document "#sample-canvas") {:grid-size 10}))

(fw/start {
           :websocket-url "ws://localhost:3449/figwheel-ws"
           :on-jsload     refresh})

(refresh)
