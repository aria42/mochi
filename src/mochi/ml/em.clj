(ns mochi.ml.em
  (:use [mochi core])
  (:require [mochi [counter :as counter]]
	    [mochi.ml [distribution :as distr]]))


(defprotocol IEMClient
  (new-stats [this params]
    "returns sufficient statistics data-structure,
     doesn't need to implement ISuffStats")
  (merge-stats [this stats other-stats]
     "merge two sets of EM statistics. used only if EM is parallelized") 
  (observe [this stats datum label post]
    "update stats according to observed datum with label and post")
  (estep-inference [this params datum]
     "Returns [log-prob posts], where posts [label,post] seq")
  (mstep-inference [this stats]
     "Return new parameters for stats"))


(defrecord SimpleEMClient
  #_{:doc "Simple EM problem where each datum has a small number
   of labels. And there is a prior distribution over labels
   and a distibution over datums for each label
   
   labels: set of labels
   prior-stat-fn: no-arg fn returns new suff-stats for prior
   suff-stats-fn: fn label => new suff-stats"}   
  [labels prior-stat-fn suff-stats-fn]
  ; Implements..
  IEMClient
  (new-stats [this _]
     [(prior-stat-fn) (make-map suff-stats-fn labels)])
  (merge-stats [this stats other-stats]
     [(distr/merge-stats (first stats) (first other-stats))
      (merge-with distr/merge-stats (second stats) (second other-stats))])     
  (observe [this stats datum label post]
    (let [prior-stats (first stats) label-stats (second stats)]       
      [(distr/obs prior-stats label post)         
       (update-in label-stats [label] distr/obs datum post)]))
  (estep-inference [this params datum]		   
    (cond
       ; Labeled datum
       (and (meta datum) (get (meta datum) :label))
          [0.0 {(-> datum meta :label) 1.0}]
       ; No Params
       (nil? params)
          [0.0 (make-map (constantly (/ 1.0 (count labels))) labels)]
       ; Unlabeled Datum
       :default
        (let [prior (first params) label-distrs (second params)]
	        (->> labels
	          (make-map
  	            ; P(label | datum) \propto P(label) P(datum | label)
                (fn [label]
                  (+ (distr/log-prob prior label)
    	             (distr/log-prob (label-distrs label) datum))))		 
	            counter/log-scores-to-probs))))
  (mstep-inference [this stats]
     [(distr/to-distribution (first stats))      
      (map-vals distr/to-distribution (second stats))]))


(defn- datum-update [em stats datum posts]
  (let [scale (if-let [scale (-> datum meta :scale)] scale 1.0)] 
    (reduce
      (fn [res [label post]]
        (observe em res datum label (* scale post)))
      stats
      posts)))
    


(defn posterior-fn [em params]
  (fn [datum] (second (estep-inference em params datum))))


(defn- serial-estep [em params datums]
      (->> datums
	       (map (fn [datum]
		            (let [[log-prob posts] (estep-inference em params datum)]    		              
		              [datum log-prob posts])))
	       (reduce
	         (fn [[sum-log-prob stats] [datum log-prob posts]]
	           (let [scale (if-let [scale (-> datum meta :scale)] scale 1.0)]  
		          [(+ sum-log-prob (* scale log-prob))
	               (datum-update em stats datum posts)]))
           [0.0 (new-stats em params)])))

(defn- parallel-estep [em params datums]
    (let [ncpus (-> (Runtime/getRuntime) .availableProcessors)
          chunks (partition-all (/ (count datums) ncpus) datums) 
          chunk-results   (pmap (partial serial-estep em params) chunks)]
      (reduce 
          (fn [[sum-log-prob stats] [log-prob chunk-stats]] 
             [(+ sum-log-prob log-prob) (merge-stats em stats chunk-stats)])
          [0.0 (new-stats em params)]
          chunk-results)))          

(defn- do-iter [em estep params datums]
  (let [[log-prob stats] (estep em params datums)] 
    [log-prob (mstep-inference em stats)]))
    
(defn run-em [em datums init-params
	            & {:keys [num-iters iter-callback]
		           :or {num-iters 5}}]      
   (println "RUNNING EM")	          
   (loop [iter 0 params init-params]
       (if (= iter num-iters) params
	      (let [[log-prob params] (do-iter em parallel-estep params datums)]	  
      	    (when iter-callback (iter-callback iter log-prob params))
      	    (recur (inc iter) params)))))
