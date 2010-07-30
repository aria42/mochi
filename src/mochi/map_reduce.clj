(ns mochi.map-reduce
  (:require [clojure.contrib seq-utils]))

(defn preduce [merge-fn
	       reduce-fn
	       post-reduce-fn
	       init-val-fn
	       chunk-size
	       xs]
  (apply merge-fn
   (pmap
    (fn [chunk] (post-reduce-fn (reduce reduce-fn (init-val-fn) chunk)))
    (partition-all chunk-size xs))))

