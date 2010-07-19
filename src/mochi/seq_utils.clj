(ns mochi.seq-utils
  (:import [java.util.regex Pattern]))

(defn iter-seq 
  "Make a seq from input fns
   hasNextFn: no-arg fn to indicate if there is a next
   nextFn: no-arg fn to return next elem"  
  [hasNextFn nextFn]
  (loop [res (transient [])]
    (if-not (hasNextFn) (persistent! res)
      (recur (conj! res (nextFn))))))

(defn singleton?
  [xs]
  (== (int (count  xs)) 1))
  
(defn sed 
  [aseq start-pred stop-pred]  
  (let [res (java.util.ArrayList.) cur (java.util.ArrayList.)]
    (doseq [x aseq]
      (when (or (start-pred x) (not (.isEmpty cur)))
        (.add cur x))
      (when (stop-pred x)
        (.add res (java.util.ArrayList. cur))
        (.clear cur)))
    (into [] (map #(into [] %) res))))