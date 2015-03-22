(ns mazes.dev
  (:require [figwheel.client :as fw]
            [mazes.core]
            [mazes.playground]
            [mazes.core-test]
            [mazes.playground-test]
            [cljs.core.async :refer [put!]]))

(defonce app-state
  (atom mazes.playground/initial-state))

(defn refresh []
  (mazes.playground/build-at (.-body js/document) app-state))

(fw/start {
           :websocket-url "ws://localhost:3449/figwheel-ws"
           :on-jsload     refresh})

(let [maze (refresh)]
  (put! (:bus maze) [:generate-maze]))
