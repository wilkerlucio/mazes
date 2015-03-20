(ns wilkerdev.util.dom
  (:require [goog.style :as style]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [cljs.core.async :refer [chan put! <! >! close!] :as async]))

; aliases for common globals

(def document js/document)
(def body (.-body document))

; query

(defn $
  ([query] ($ document query))
  ([target query] (.querySelector target query)))

(defn $$
  ([query] ($$ document query))
  ([target query] (array-seq (.querySelectorAll target query))))

(defn by-id [id] (.getElementById document id))

; style

(defn has-class? [el name]
  (classes/has el name))

(defn set-class! [el name]
  (classes/set el name))

(defn add-class! [el name]
  (classes/add el name))

(defn remove-class! [el name]
  (classes/remove el name))

(defn toggle-class!
  ([el name] (toggle-class! el name (not (has-class? el name))))
  ([el name bool]
   (if bool
     (add-class! el name)
     (remove-class! el name))))

(defn set-style!
  ([el styles] (style/setStyle el (clj->js styles)))
  ([el style value] (style/setStyle el (name style) value)))

; manipulation

(defn create-element [name]
  (dom/createElement name))

(defn insert-after! [el target]
  (doto el (dom/insertSiblingAfter target)))

(defn insert-before! [el target]
  (doto el (dom/insertSiblingBefore target)))

(defn append! [parent el]
  (doto parent (dom/append el)))

(defn append-to! [el parent]
  (append! parent el)
  el)

(defn append-all! [parent els]
  (doseq [el els] (append! parent el))
  parent)

(defn remove-node! [el]
  (dom/removeNode el))

(defn replace-node! [new-node old-node]
  (dom/replaceNode new-node old-node))

(defn set-html! [el s]
  (set! (.-innerHTML el) s))

(defn set-properties! [el properties]
  (doto el (dom/setProperties (clj->js properties))))

; traversing

(defn ancestor [el matcher]
  (dom/getAncestor el matcher))

(defn tag-match [tag]
  (fn [el]
    (when-let [tag-name (.-tagName el)]
      (= tag (.toLowerCase tag-name)))))

(defn parent [el tag]
  (let [matcher (tag-match tag)]
    (if (matcher el)
      el
      (dom/getAncestor el (tag-match tag)))))

(defn el-matcher [el]
  (fn [other] (identical? other el)))

(defn query-matcher [query]
  (fn [el]
    (if (and el (.-matches el)) (.matches el query))))

; dom mutation

(defn observe-mutation* [{:keys [callback container options]
                         :or   [container body
                                options {}]}]
  (let [win js/window
        klass (or (.-MutationObserver win)
                  (.-WebKitMutationObserver win)
                  (.-MozMutationObserver win)
                  nil)]
    (if klass
      (doto (klass. callback)
            (.observe container (clj->js (merge {:childList     true
                                                 :attributes    true
                                                 :characterData true
                                                 :subtree       false}
                                                options)))))))

(defn observe-mutation
  ([options] (observe-mutation options (chan)))
  ([options c]
    (observe-mutation* (merge options {:callback (fn [mutations] (put! c mutations))}))
    c))

; video

(defn video-current-time [video] (.-currentTime video))
(defn video-duration [video] (.-duration video))
(defn video-seek! [video time] (set! (.-currentTime video) time))
(defn video-playback-rate! [video rate] (set! (.-playbackRate video) rate))
