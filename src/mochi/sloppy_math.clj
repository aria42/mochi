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

(defn- small-digamma [x]
  (+ (Math/log x)
     (* 0.04167 (Math/pow x -2))
     (* -0.00729 (Math/pow x -4))
     (* 0.00384 (Math/pow x -6))
     (* -0.00413 (Math/pow x -8))))

(defn digamma [x]
  (if (> x 7)
    (small-digamma (- x 0.5))
    (- (digamma (inc x)) (/ 1.0 x))))

(comment
  (Math/exp (log-add [0 0 0 0 0 0]))
  (digamma 1)
)	      	      
	       		  
		
		
	    
	    
	  

  