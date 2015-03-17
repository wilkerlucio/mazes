(ns wilkerdev.browsers.chrome)

(defn resource-url [path]
  (.. js/chrome -extension (getURL path)))

(defn i18n [] (.-i18n js/chrome))

(defn i18n-message [name] (.getMessage (i18n) name))
