(ns mochi.sloppy-math)

(defn- my-max [xs]
  (loop [i 0 xs xs max Double/NEGATIVE_INFINITY arg-max -1]
    (if-let [x (first xs)]
      (if (> x max) 
	(recur (inc i) (rest xs) (double x) i)
	(recur (inc i) (rest xs) 
max arg-max))
      [arg-max max])))

(def *log-thresh* 30.0)

(defn log-add [xs]
  (let [[arg-max max] (my-max xs) thresh (- max *log-thresh*)]
    (if (= max Double/NEGATIVE_INFINITY) Double/NEGATIVE_INFINITY
	(let [sum-neg-diffs (reduce + (for [x xs :when (> x thresh)] (Math/exp (- (double x) (double max)))))]
	  (if (> sum-neg-diffs 0.0) (+ max (Math/log (double sum-neg-diffs)))
	      max)))))
	       		  
		
		
	    
	    
	  

  