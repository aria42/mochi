;; (ns mochi.ml.feature-multinomial
;;   {:doc "A feature-based multinomial distribution. "
;;    :author "Aria Haghighi (aria42@gmail.com)"}
;;   (:use
;;    [mochi.array-utils :only [double-ainc!]]
;;    [mochi core])
;;   (:require
;;    [mochi [sloppy-math :as sloppy-math] [counter :as counter]]
;;    [mochi.ml [distribution :as distr]])
;;   (:import
;;    [edu.umass.nlp.utils DoubleArrays IPair BasicPair]
;;    [edu.umass.nlp.exec Execution]
;;    [edu.umass.nlp.optimize
;;      IOptimizer  LBFGSMinimizer
;;      IDifferentiableFn CachingDifferentiableFn]
;;    [edu.umass.nlp.ml Regularizers]
;;    [mochi.ml.distribution ISuffStats IDistribution]))

;; ;;; Train ;;;

;; (defrecord FeatValPair [#^int index #^double val])

;; (defrecord EventInfo [fvs #^double weight])

;; (defn- event-potential-fn
;;   "returns f: fv -> weights * fv"
;;   [#^doubles weights]
;;   (fn [#^EventInfo event-info]
;;     (sum
;;      (fn [#^FeatValPair fv] (* (aget weights (.index fv)) (.val fv)))
;;      (.fvs event-info))))

;; (defn- make-event-probs [event-info-map weights]
;;   (->> event-info-map
;;        (map-vals (event-potential-fn weights))
;;        counter/log-scores-to-probs
;;        second))
	    
;; (defn- update-gradient! [gradient  #^EventInfo event-info model-prob]
;;   (let [empirical (.weight event-info)]
;;     (doseq [#^FeatValPair fv (.fvs event-info)
;; 	    :let [index (.index fv) val (.val fv)]]
;;       (double-ainc! gradient index (* val (- empirical  model-prob))))))

;; (defn- objective-compute-helper [event-info-map  #^doubles weights opts]
;;   (let [event-probs (make-event-probs event-info-map weights)
;; 	    gradient (double-array (alength weights))]
;;     ; return [log-prob gradient] pair 
;;     [(sum
;;           (fn [[event #^EventInfo event-info]]
;;     	    (let [event-prob (event-probs event)]
;;     	      (update-gradient! gradient event-info event-prob)
;;     	      (* (.weight event-info) (Math/log event-prob))))
;;           event-info-map)
;;      gradient]))

;; (defn- wrap-regularizer [weights neg-log-prob #^doubles gradient sigma-sq]
;;   (let [reg-fn (Regularizers/getL2Regularizer sigma-sq)
;; 	#^IPair reg-pair (.apply reg-fn weights)]
;;     (DoubleArrays/addInPlace gradient #^doubles (.getSecond reg-pair))
;;     (BasicPair/make
;;      (+ neg-log-prob (.getFirst reg-pair))
;;      gradient)))

;; (defn- objective-compute [event-info-map weights opts]
;;   (let [[log-prob gradient] (objective-compute-helper event-info-map weights opts)]
;;     (DoubleArrays/scaleInPlace gradient -1)
;;     (wrap-regularizer weights (- log-prob) gradient (:sigma-sq opts))))

;; (defn- feat-indexing [feat-fn events]
;;   (indexer (for [e events [f _] (feat-fn e)] f)))

;; (defn- make-event-info-map [feat-fn  event-counts]
;;   (let [[feats feat-indexer] (feat-indexing feat-fn (map first event-counts))]
;;     [(into {}
;; 	   (for [[event count] event-counts]
;; 	     [event
;; 	      (EventInfo.
;; 	       (for [[f v] (feat-fn event)
;; 		     :let [index (feat-indexer f)]
;; 		     :when (>= index 0)] (FeatValPair. index v))
;; 	       count)]))
;;       feats]))

;; (defn- do-optimization [feats event-info-map opts]
;;   (-> (LBFGSMinimizer.)
;;       (.minimize
;;           (CachingDifferentiableFn.
;; 	   (reify IDifferentiableFn
;; 		  (#^IPair computeAt [this #^doubles weights]
;; 			   (objective-compute event-info-map weights opts))
;; 		  (#^int getDimension [this] (count feats))))
;; 	  (double-array (count feats))
;; 	  (doto (edu.umass.nlp.optimize.LBFGSMinimizer$Opts.)
;; 	    (set-field! minIters 1)
;; 	    (set-field! logLevel (:log-level opts))))
;;       .minArg))

;; (defn- train-multinomial [feat-fn event-counts train-opts]
;;   (let [[event-info-map feats] (make-event-info-map feat-fn event-counts)
;; 	#^doubles weights (do-optimization feats event-info-map train-opts)]
;;     (make-event-probs event-info-map weights)))
	  
;; ;;; Feat Multinomial ;;;

;; (defrecord FeatMultinomialDistribution
;;   [potential-fn log-sum]
;;   IDistribution
;;   (log-prob [this event]
;;       (- (potential-fn event) log-sum)))

;; (defrecord FeatMultinomialSuffStats
;;   [event-counts feat-fn train-opts]

;;   ISuffStats
;;   (obs [this event weight]
;;    (FeatMultinomialSuffStats.
;;     (counter/inc-count event-counts event weight)
;;     feat-fn
;;     train-opts))
;;   (merge-stats [this other]
;;    (FeatMultinomialSuffStats.
;;     (counter/merge-counters event-counts (.event-counts #^FeatMultinomialSuffStats other))
;;     feat-fn
;;     (merge train-opts (.train-opts #^FeatMultinomialSuffStats other))))
;;   (to-distribution [this] 
;;       #_(println "event-counts: " event-counts)
;;       (let  [res (train-multinomial feat-fn event-counts train-opts)]
;;         #_(println "result: " res)
;;         res)))

;; ;;; Factory ;;;

;; (def ^:private default-opts
;;      { :sigma-sq 1.0
;;        :log-level org.apache.log4j.Level/INFO
;;        :parallel true})

;; (defn new-suff-stats
;;   "Supported train-opts supports keyword map:
;;      sigma-sq [default 1.0]
;;      log-level [defailt INFO]
;;      parallel [default true]"
;;   [feat-fn & {:as train-opts}]
;;   (FeatMultinomialSuffStats. (counter/make) feat-fn (into default-opts train-opts)))

;; (defn make [feat-fn event-counts & {:as train-opts}]
;;   (distr/to-distribution (FeatMultinomialSuffStats. event-counts feat-fn train-opts)))

;; ;;; Testing ;;;

;; (defn- -main [& ignore]
;;   (Execution/init nil)
;;   (def f (fn [x] [[(format "Identity:%s" x) 1.0]
;; 		  #_[(format "BIAS") 1.0]
;; 		  #_[(format "FirstChar:%s" (first x)) 1.0]
;; 		  #_[(format "LastChar:%s" (last x)) 1.0]]))
;; 	; Event Counts
;;   (def event-counts (counter/normalize {"aria" 0.1 "s" 0.01 "baria" 7 "daria" 1}))
;;   (seq (double-array [(Math/log 1) (Math/log 7) (Math/log 1) (Math/log 1)]))
;;   (def ss (distr/obs-counter (new-suff-stats f :sigma-sq 100000)
;; 			     event-counts))
;;   (distr/to-distribution ss) 
;;   ; 
;;   )

