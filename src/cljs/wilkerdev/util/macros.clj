(ns wilkerdev.util.macros
  (:refer-clojure :exclude [test])
  (:require [cljs.core.async.macros :refer [go-loop]]))

(defmacro dochan [[binding chan] & body]
  `(let [chan# ~chan]
     (cljs.core.async.macros/go
       (loop []
         (if-let [~binding (cljs.core.async/<! chan#)]
           (do
             ~@body
             (recur))
           :done)))))

(defmacro <? [ch]
  `(wilkerdev.util.reactive/throw-err (cljs.core.async/<! ~ch)))

(defmacro go-catch [& body]
  `(cljs.core.async.macros/go
     (try
       ~@body
       (catch js/Error e e))))

(defmacro bench [message & body]
  `(do
     (.time js/console ~message)
     (let [res# (do ~@body)]
       (.timeEnd js/console ~message)
       res#)))

(defmacro all-or-nothing [expr & forms]
  (let [g (gensym)
        pstep (fn [step] `(if (nil? (last ~g)) nil (conj ~g ~step)))]
    `(let [~g [~expr]
           ~@(interleave (repeat g) (map pstep forms))
           ~g (if (nil? (last ~g)) nil ~g)]
       ~g)))

(defmacro go-sub* [pub key binding c & body]
  `(let [ch# ~c]
     (cljs.core.async/sub ~pub ~key ch#)
     (go-loop []
       (when-let [~binding (cljs.core.async/<! ch#)]
         ~@body
         (recur)))))

(defmacro go-sub [pub key binding & body]
  `(go-sub* ~pub ~key ~binding (cljs.core.async/chan 1) ~@body) )

(defmacro deblog [& body]
  `(do
     (let [s# (pr-str '~body)]
       (.log js/console (subs s# 1 (dec (count s#)))))
     ~@body))
