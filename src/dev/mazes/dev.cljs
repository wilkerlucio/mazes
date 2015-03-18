(ns mazes.dev
  (:require [figwheel.client :as fw]
            [mazes.core]
            [mazes.playground]
            [mazes.core-test]
            [cemerick.cljs.test :as t]
            [cljs.core.async :refer [put!]]))

(defn refresh []
  (mazes.playground/build-at (.getElementById js/document "app-container")))

(fw/start {
           :websocket-url "ws://localhost:3449/figwheel-ws"
           :on-jsload     refresh})

(let [maze (refresh)]
  (put! (:bus maze) [:generate-maze]))
