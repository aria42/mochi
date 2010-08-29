(ns mochi.vec-utils
  (:require [clojure.contrib [seq-utils :as su]]))


(defn ensure-vec
  "returns vector of xs, O(1) if already vector"
  [xs]
  (if (vector? xs) xs (vec xs)))


(defn index-where
  "Argument should be a vector, or will be converted to one"
  [xs pred]
  (first (for [[i x] (su/indexed (ensure-vec xs)) :when (pred x)] i)))
