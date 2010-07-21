(ns mochi.ml.feature-multinomial
  {:doc "A feature-based multinomial distribution. "
   :author "Aria Haghighi (aria42@gmail.com)"}
  (:use
   [mochi.array-utils :only [double-ainc!]]
   [mochi core])
  (:require
   [mochi [sloppy-math :as sloppy-math] [counter :as counter]]
   [mochi.ml [distribution :as distr]])
  (:import
   [edu.umass.nlp.utils DoubleArrays IPair BasicPair]
   [edu.umass.nlp.exec Execution]
   [edu.umass.nlp.optimize
     IOptimizer  LBFGSMinimizer
     IDifferentiableFn CachingDifferentiableFn]
   [mochi.ml.distribution ISuffStats IDistribution]))

;;; Train ;;;

(defrecord FeatValPair [#^int index #^double val])

(defrecord EventInfo [fvs #^double weight])

(defn- make-log-scores [event-info-map #^doubles weights]
  (map-vals
   (fn [event-info]
     (sum
          (fn [#^FeatValPair fv] (* (aget weights (.index fv)) (.val fv)))
	  (.fvs event-info)))
   event-info-map))

(defn- make-event-probs [event-info-map weights]
  (-> (make-log-scores event-info-map weights)
      counter/log-scores-to-probs
      second))
	     
(defn- update-gradient! [gradient  event-info event-prob]
  (let [c (.weight event-info)]
    (doseq [#^FeatValPair fv (.fvs event-info)
	    :let [index (.index fv)
		  val   (.val fv)]]
      #_(swank.core/break)
      (double-ainc! gradient index (* c val (- 1.0 event-prob))))))

(defn- objective-compute-helper [event-info-map  weights]
  (let [event-probs (make-event-probs event-info-map weights)
	gradient (double-array (alength weights))]
    ; return [log-prob gradient] pair 
    [(sum 
      (fn [[event #^EventInfo event-info]]
	(let [#^double event-prob (event-probs event)]
	  (update-gradient! gradient event-info event-prob)
	  (* (.weight event-info) (Math/log event-prob))))
      event-info-map)
     gradient]))

(defn- objective-compute [event-info-map weights]
  (let [[log-prob gradient] (objective-compute-helper event-info-map  weights)]
    #_(do (println event-probs)
	  (println log-prob)
	  (println (seq gradient))	
	  (swank.core/break))
    (DoubleArrays/scaleInPlace gradient -1)
    (BasicPair/make (- log-prob) gradient)))

(defn- feat-indexing [feat-fn events]
  (indexer (for [e events [f _] (feat-fn e)] f)))

(defn- make-event-info-map [feat-fn  event-counts]
  (let [[feats feat-indexer] (feat-indexing feat-fn (map first event-counts))]
    [(into {}
	   (for [[event count] event-counts]
	     [event
	      (EventInfo.
	       (for [[f v] (feat-fn event)
		     :let [index (feat-indexer f)]
		     :when (>= index 0)] (FeatValPair. index v))
	       count)]))
      feats]))

(defn- do-optimization [feats event-info-map]
  (-> (LBFGSMinimizer.)
      (.minimize
          (CachingDifferentiableFn.
	   (reify IDifferentiableFn
		  (#^IPair computeAt [this #^doubles weights]
			   (objective-compute event-info-map weights))
		  (#^int getDimension [this] (count feats))))
	  (double-array (count feats))
	  nil)
      .minArg))

(defn train-multinomial [feat-fn event-counts]
  (let [[event-info-map feats] (make-event-info-map feat-fn event-counts)
	#^doubles weights (do-optimization feats event-info-map)]
    (make-event-probs event-info-map weights)))
	  
;;; Feat Multinomial ;;;

(defrecord FeatMultinomialSuffStats
  [event-counts feat-fn]

  ISuffStats
  (obs [this event weight]
   (FeatMultinomialSuffStats.
    (counter/inc-count event-counts event weight)
    feat-fn))
  (merge-stats [this other]
   (FeatMultinomialSuffStats.
    (counter/merge-counters (.event-counts this) (.event-counts other))
    feat-fn))
  (to-distribution [this] (train-multinomial feat-fn event-counts)))

;;; Factory ;;;

(defn new-suff-stats [feat-fn]
  (FeatMultinomialSuffStats. (counter/make) feat-fn))

(defn make [feat-fn event-counts]
  (distr/to-distribution (FeatMultinomialSuffStats. event-counts feat-fn)))

;;; Testing ;;;

(defn- run [& ignore]
  (Execution/init nil)
  (def f (fn [x] [[(format "Identity:%s" x) 1.0]
		  [(format "FirstChar:%s" (first x)) 1.0]
		  [(format "LastChar:%s" (last x)) 1.0]]))

  (def event-counts {"aria" 10.0 "baria" 3.0 "bas" 1.0
				 "daria" 3.0 "saria" 2.0
				 "sam" 20.0
				 "sally" 3.0
				 "santa" 1.0})
  
  (println (train-multinomial f event-counts))
  (println (distr/make-DirichletMultinomial
	    :counts (counter/make event-counts)
	    :lambda 0.0)))

;(when *command-line-args* (apply run *command-line-args*))
