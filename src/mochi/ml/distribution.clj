(ns mochi.ml.distribution
  (:use [clojure.contrib [def :only [defnk]]])
  (:require
   [mochi [counter :as cntr]          
          [core :as core]]))

;;; Suff Stats ;;;

(defprotocol ISuffStats
  "Statistics for a distribution"
  (obs [this e w] "observe weighted event e")
  (merge-stats [this other] "merge sufficient statistics")
  (to-distribution [this] "make an IDistribution from suff stats"))

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

;;; Distribution ;;;

(defprotocol IDistribution
  "Distribution Abstraction, many have default implementations
   in default-distr-impl"
  (events [this] "seq of [event prob] pairs") 
  (prob [this  e] "probability of event e")
  (log-prob [this e] "log prob of event")
  (mode [this] "mode of distribution")
  (sample [this rand] [this] "sample distribution")
  (support [this] "support for a distribution"))

(def default-distr-impl
 {:events ; assumes when you seq the object you get events
     (fn [this] (seq this))
  :support ; assumes events is defined
     (fn [this] (map first (events this)))
  :log-prob ; just log the probability
     (fn [this e] (Math/log (prob this e)))
  :mode ; manual search for mode
     (fn [this] (first (apply max-key second (events this))))
  :sample ; look at all events and sample
     (fn sample
       ([this] (sample this (java.util.Random.)))
       ([this #^java.util.Random rand]
	  (let [x (.nextDouble rand)]
	    (loop [start 0.0 probs (events this)]	      
	      (if (empty? probs) (throw (IllegalStateException.))
		  (let [[e prob] (first probs)
			stop (+ start prob)]
		    (if (and (>= x start) (<= x stop)) e
			(recur stop (rest probs)))))))))})

(extend  clojure.lang.IPersistentMap
  IDistribution
  (into default-distr-impl
   { :prob (fn [this e] (get this e 0.0))} ))

(extend  mochi.counter.Counter
  IDistribution
  (into default-distr-impl
	{ :prob (fn [this e]
		  (let [total (cntr/total-count this)]
		    (if (zero? total) 0.0
			(/ (this e) total))))}))

;;; Dirichlet Multinomial ;;;

(defn- laplace-smoothed-prob [count lambda total num]
  (/ (+ count lambda) (+ total (* num lambda))))

(deftype DirichletMultinomial [counts lambda numKeys]
  java.lang.Object
  (toString [this] (-> this events vec str))

  clojure.lang.Seqable
  (seq  [this] (events this))

  clojure.lang.IFn
  (invoke  [this e] (prob this e)))

(extend DirichletMultinomial
  
  ISuffStats
  { :obs
     (fn [this e w]
       (let [new-counts (cntr/inc-count (.counts this) e w)]
	 (DirichletMultinomial.
	  new-counts
	  (.lambda this)
	  (max (count new-counts) (.numKeys this)))))
   :merge-stats
      (fn [this other]
	(let [new-counts
	      (cntr/merge-counters  (.counts this) (.counts other))]
	  (DirichletMultinomial.
	     new-counts
	     (.lambda this)
	     (max (count new-counts) (.numKeys this)))))
   :to-distribution
     (fn [this] this)
   }

  IDistribution
  (into default-distr-impl
   {:events (fn [this] 
     (let [total (cntr/total-count (.counts this))]
       (for [[k c] (.counts this)] 
	[k (laplace-smoothed-prob c (.lambda this) total  (.numKeys this))])))
    
    :prob (fn [this e]
      (laplace-smoothed-prob (-> this .counts (cntr/get-count e))
			     (.lambda this)
			     (-> this .counts cntr/total-count)
			     (.numKeys this)))}))
           
(defnk make-DirichletMultinomial
  [:counts (cntr/make) :lambda 0.0 :numKeys -1]
  (let [numKeys (if (> numKeys 0) numKeys (count (seq counts)))]
    (DirichletMultinomial. counts lambda numKeys)))

;;; Testing ;;;

(comment
  (def d (make-DirichletMultinomial :counts (cntr/make {:a 1.0}) :lambda 1.0))
  (def e (make-DirichletMultinomial :counts (cntr/make {:b 2.0}) :lambda 1.0))
  (def d (merge-stats d e))
  (log-prob d :a)
  (obs d :a 2.0) 
  (to-distribution (obs d :a 2)) 
  (mode d)
)