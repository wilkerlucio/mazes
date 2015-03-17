(ns wilkerdev.local-storage
  (:refer-clojure :exclude [get set!])
  (:require [cljs.reader :refer [read-string]]))

(def local-storage (.-localStorage js/window))

(defn get
  ([key] (get key nil))
  ([key default]
    (if-let [value (.getItem local-storage (name key))]
      (read-string value)
      default)))

(defn set! [key value]
  (.setItem local-storage (name key) (pr-str value)))

(defn remove! [key]
  (.removeItem local-storage key))
