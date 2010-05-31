;(ns mochi.map-reduce
;  (:require [clojure.contrib seq-utils]))
;
;(defn map-reduce
;  [xs group-fn map-fn reduce-fn]
;  (persistent! (reduce
;    (fn [res x]
;      (let [gx (group-fn x) mx (map-fn x)]
;        (if-let [v (res gx)]
;          (assoc! res gx (reduce-fn v mx))
;          (assoc! res gx mx))))
;    (transient {}) xs)))
;
;(defn chunk-pmap [f coll chunk-size]
;  (apply concat
;    (pmap (fn [chunk] (map f chunk)) (partition-all chunk-size coll))))
;
;(defn pmap-reduce
;  [xs chunk-size group-fn map-fn reduce-fn]
;  (reduce (fn [x y] (merge-with reduce-fn x y)) {}
;    pmap #(map-reduce % group-fn map-fn reduce-fn) (partition-all chunk-size xs)))
;
;(defn map-reduce-by
;  "Returns a hash map where elements of coll are mapped by map-fn
;and then reduced by reduce-fn. The map is keyed by the result
;of group-fn on the original element. If specified, val is the
;initial value of the reduce operation."
;  ([group-fn map-fn reduce-fn coll]
;    (reduce
;      (fn [m x]
;        (let [group (group-fn x)
;              mx (map-fn x)
;              pair (find m group)]
;          (assoc m group (if pair (reduce-fn (val pair) mx) mx))))
;      {} coll))
;  ([group-fn map-fn reduce-fn val coll]
;    (println "group" group-fn "map" map-fn "reduce" reduce-fn "val"
;      val "coll" coll)
;    (reduce
;      (fn [m x]
;        (let [group (group-fn x)]
;          (assoc m group (reduce-fn (get m group val) (map-fn x)))))
;      {} coll)))
;
;
;(defn freqs
;  [xs]
;  (map-reduce xs identity (constantly 1.0) +))
;
;(defn pfreqs
;  [xs]
;  (pmap-reduce xs 100000 identity (constantly 1.0) +))
;
