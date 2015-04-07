(ns mazes.main
  (:require [mazes.playground]
            [cljs.core.async :refer [put!]]))

(def app-state (atom mazes.playground/initial-state))

(let [{:keys [bus]} (mazes.playground/build-at (.getElementById js/document "app-container") app-state)]
  (put! bus [:generate-maze]))
