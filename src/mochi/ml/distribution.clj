(ns mochi.ml.distribution
  (:use [clojure.contrib [def :only [defnk]]])
  (:require [mochi.counter :as cntr]
	    [mochi.core :as mc]))

;;; -----------------
;;; Suff Stats
;;; -----------------

(defprotocol ISuffStats
  "Statistics for a distribution"
  (obs [this e w] "observe weighted event e")
  (merge-stats [this other] "merge sufficient statistics")
  (to-distribution [this] "make a distribution from suff stats"))


(extend-protocol ISuffStats

  mochi.counter.Counter
  (obs [c e w] (cntr/inc-count c e w)))

(defn obs-all 
  "(obs-all suff-stats :a 1.0 :b 2.0)"
  [suff-stats & kvs]
  (reduce (fn [res [k v]] (obs res k v))
	  suff-stats
	  (partition 2 kvs)))

(defn obs-counter
  "observe all events in the counter arg"
  [suff-stats counter]
  (apply obs-all suff-stats (mapcat (seq counter))))

;;; -----------------
;;; Distribution
;;; -----------------

(defprotocol IDistribution
  "Distribution Abstraction"
  (events [_] "seq of [event prob] pairs") 
  (prob [_ e] "probability of event e"))

(defn support 
  "support of a distribution"
  [distr] 
  (map first (events distr)))

(extend-protocol IDistribution

 clojure.lang.IPersistentMap
 (events [c] (seq c))
 (prob [c e] (get c e 0.0))
  
 mochi.counter.Counter
 (events [c] (seq c))
 (prob [c e] (/ (c e) (cntr/total-count c))))

;;; -----------------------
;;; Dirichlet Multinomial
;;; -----------------------

(defn- laplace-smoothed-prob [count lambda total num]
  (/ (+ count lambda) (+ total (* num lambda))))


(deftype DirichletMultinomial [counts lambda numKeys]
  
  ISuffStats
  (obs [this e w]
    (let [new-counts (cntr/inc-count counts e w)]
      (DirichletMultinomial.
       new-counts
       lambda
       (max (count new-counts) numKeys))))
  (merge-stats [this other]
    (let [new-counts (cntr/merge-counters (.counts this) (.counts other))]
      (DirichletMultinomial. new-counts lambda (max (count new-counts) numKeys))))
  
  IDistribution
  (events [this] 
    (for [[k c] counts] 
      [k (laplace-smoothed-prob c lambda (.total counts) numKeys)]))
      
  (prob [this e] 
    (laplace-smoothed-prob (counts e)
			   lambda
			   (cntr/total-count counts)
			   numKeys))

  java.lang.Object
  (toString [this] (-> this events vec str))
  
  clojure.lang.Seqable
  (seq [this] (events this))

  clojure.lang.IFn
  (invoke [this e] (prob this e)))  

           
(defnk make-DirichletMultinomial
  [:counts (cntr/make) :lambda 0.0 :numKeys -1]
  (let [numKeys (if (> numKeys 0) numKeys (count (seq counts)))]
    (DirichletMultinomial. counts lambda numKeys)))

;;; -----------------
;;; etc....
;;; -----------------


(comment
  {:counts (cntr/make {:a 1.0}) :lambda 1.0}
  (def c (cntr/Counter.))
  (prob c :a)
  (obs c :a 1.0)
  (def d (make-DirichletMultinomial :counts (cntr/make {:a 1.0}) :lambda 1.0))
  (cntr/merge-counters (.counts d) (.counts d))
  (merge-stats d d)
  (prob d :a)
  (events d)
  (events (obs d :b 1.0))
  (events (obs-all d :a, 2.0 :b 3.0))
  (partition 2 [:a, 2.0 :b 3.0])
  (seq (obs-all d :a, 2.0 :b 4.0))
  (.counts (make-DirichletMultinomial :lambda 1.0))
)