(ns mochi.map-reduce
  (:require [clojure.contrib seq-utils]))

(defn preduce [merge-fn reduce-fn init-val chunk-size xs]
  (reduce merge-fn
   (pmap
    (fn [chunk] (reduce reduce-fn init-val chunk))
    (partition-all chunk-size xs))))

