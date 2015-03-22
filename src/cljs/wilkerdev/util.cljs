(ns wilkerdev.util
  (:require [goog.string :as str]
            [goog.string.format]))

(defn mapply
  "Applies a function f to the argument list formed by concatenating
  everything but the last element of args with the last element of
  args. This is useful for applying a function that accepts keyword
  arguments to a map."
  {:arglists '([f & args])}
  ([f m]        (apply f (apply concat m)))
  ([f a & args] (apply f a (apply concat (butlast args) (last args)))))

(defn format [fmt & args]
  "Formats a string using goog.string.format."
  (apply goog.string/format fmt args))

(defn js->map [obj]
  "Forces a js->map convertion but only on the first level of nesting."
  (->> (js-keys obj)
       (map #(vector % (aget obj %)))
       (map #(update-in % [0] (comp keyword str/toSelectorCase)))
       (into {})))

(defn map->query [m]
  "Converts a map into a query string."
  (->> (clj->js m)
       (.createFromMap goog.Uri.QueryData)
       (.toString)))

(defn quote-regexp [string]
  "Quotes regexp characters from a string, making it suitable to use as literals in Regexp"
  (.replace string (js/RegExp "[-\\^$*+?.()|[\\]{}]" "g") "\\$&"))

(defn distinct-consecutive
  "Returns a lazy sequence of the elements of coll with consecutive duplicates removed"
  ([]
   (fn [rf]
     (let [last-seen (volatile! ::unseen)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (= @last-seen input)
            result
            (do (vreset! last-seen input)
                (rf result input))))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] last-seen]
                     (when-let [s (seq xs)]
                       (if (= last-seen f)
                         (recur (rest s) last-seen)
                         (cons f (step (rest s) f)))))
                    xs seen)))]
     (step coll ::unseen))))
