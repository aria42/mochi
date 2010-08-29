(ns mochi.sloppy-math
  (:require [clojure.contrib.seq-utils :as cc.seq-utils]))

(defn- my-max [xs]
  (if (empty? xs) [-1 Double/NEGATIVE_INFINITY]
      (apply max-key (comp max second) (cc.seq-utils/indexed xs))))

(def *log-thresh* 30.0)

(defn log-add [xs]
  (let [[arg-max max] (my-max xs) thresh (- max *log-thresh*)]
    (if (= max Double/NEGATIVE_INFINITY) Double/NEGATIVE_INFINITY
	(let [sum-neg-diffs (reduce + (for [x xs :when (> x thresh)] (Math/exp (- (double x) (double max)))))]
	  (if (> sum-neg-diffs 0.0) (+ max (Math/log (double sum-neg-diffs)))
	      max)))))
	      
(comment
  (Math/exp (log-add [0 0 0 0 0 0]))
)	      	      
	       		  
		
		
	    
	    
	  

  