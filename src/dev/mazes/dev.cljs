(ns mazes.dev
  (:require [figwheel.client :as fw]
            [mazes.core-test]
            [cemerick.cljs.test :as t]))

(fw/start {
  :websocket-url "ws://localhost:3449/figwheel-ws"
  :on-jsload (fn []
               )})
