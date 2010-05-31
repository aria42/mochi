(ns mochi.span
  (:refer-clojure :exclude [contains?]))

(defprotocol ISpan 
  (start [s] "start")
  (stop [s] "stop"))

(extend-protocol  ISpan

  clojure.lang.IPersistentVector
  (start [p] (first p))
  (stop [p] (second p)))

(defn length [s] (- (stop s) (start s)))

(defn to-pair [s] [(start s) (stop s)])

(defn contains? [x y] 
  (let [[s1 t1] [(start x) (stop x)]
	[s2 t2] [(start y) (stop y)]]
    (and (<= s1 s2) (>= t1 t2))))

(defn intersect [x y]
  (let [[s1 t1] (to-pair x)
	[s2 t2] (to-pair y)
	s (max s1 s2)
	t (min t1 t2)]
    (when (> t s) [s t])))
         
(comment
  (start [0 5])
  (intersect [0 5] [0 3])
)		      
