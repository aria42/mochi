(ns mochi.ml.distribution.structured
  (use [mochi core] [mochi.ml distribution]))



;; Each observation is a key-value map
;; where keys match distr-map input
(defrecord IndepJointDistribution [distr-map]
  IDistribution
  (log-prob [this obs-map]
   (sum
     (fn [[key val]]
       (-> distr-map (get key) (log-prob val)))
     obs-map)))


(deftype IndepJointSuffStats [suff-stats-map]
  ISuffStats
  (obs [this obs-map weight]
    (assert (instance? clojure.lang.IPersistentMap obs-map))   
    (->> obs-map
	    (reduce
            (fn [res [key val]]
                (update-in res [key] obs val weight))
            suff-stats-map)
	    (IndepJointSuffStats.))) 
  (merge-stats [this other]
    (IndepJointSuffStats. (merge-with merge-stats suff-stats-map (.suff-stats-map #^IndepJointSuffStats  other))))
  (to-distribution [this]
     (IndepJointDistribution. (map-vals to-distribution suff-stats-map))))


(defn make-suff-stats-map [suff-stats-map]
    (IndepJointSuffStats. suff-stats-map))


(defrecord IndepListDistribution [distr]
  IDistribution
  (log-prob [this os]
      (sum (partial log-prob distr) os))
  java.lang.Object
  (toString [this] (str distr)))


;; Generate multiple observations
;; from the same distribution
(deftype IndepListSuffStats [suff-stats]
  ISuffStats
  (obs [this os weight]
    (IndepListSuffStats. 
        (reduce 
            (fn [res o] (obs res o weight)) 
            suff-stats
            os)))
  (to-distribution [this]
    (IndepListDistribution.  (to-distribution suff-stats)))
  (merge-stats [this other]
    (IndepListSuffStats. (merge-stats suff-stats (.suff-stats #^IndepListSuffStats other))))
  ;
  java.lang.Object
  (toString [this] (str suff-stats)))


(defn make-list-suff-stats [suff-stats]
  (IndepListSuffStats. suff-stats))  
    
(comment
  (def ss (obs (IndepListSuffStats. (make-DirichletMultinomial)) [:a :b :c :a] 1.0))  
  (def dd (to-distribution ss))
  (.distr dd)
  (Math/exp (log-prob dd [:a :a]))
)    

