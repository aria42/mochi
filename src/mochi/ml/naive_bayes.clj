(ns mochi.ml.naive-bayes
  (:require 
    [mochi.core :as mc]
    [mochi.sloppy-math :as ss]
    [mochi.counter :as cntr]
    [mochi.ml.distribution :as distr]))

;;; -----------------
;;; Data
;;; -----------------

(defprotocol IDatum
  "Data supports a method to get at [f,v] pairs"
  (feat-vec [d] "vec of [f,v] pairs"))

(defprotocol ILabeled
  "Something Labeled just supports a label method"
  (label [d] "label of the datum"))

;; Default Implementation assumes vector [label feat-vec]
(extend-type clojure.lang.IPersistentVector
  IDatum
  (feat-vec [x] (second x))
  ILabeled
  (label [x] (first x)))

;;; --------------
;;;  Params
;;; --------------

(deftype Params [labelToFeatDistrs labelPrior])

(def default-feat-distr
  (reify
    distr/IDistribution
    (prob [this e] 1.0e-20)))
  
(defn all-labels [params] (distr/support (.labelPrior params)))

(defn feat-distr [params label]  
  (fn [f] (-> params :labelToFeatDistrs (get label) (get f default-feat-distr))))

(defn obs-log-prob
  "given datum, f: label -> log P(datum|label)"
  [params datum]
  (let [fvs (feat-vec datum) distr-fn (feat-distr params label)]
    (fn [label] 
      (reduce + (for [[f v] fvs] (Math/log (distr/prob (distr-fn f) v)))))))

(defn log-prior
  "f: label -> log P(label)"
  [params]
  (fn [label] (Math/log (distr/prob (:labelPrior params) label))))

(defn datum-log-prob
  "given datum, f: label -> log P(datum,label)"
  [params datum]
  (let [prior (log-prior params) obs (obs-log-prob params datum)]
    (fn [label] (+ (prior label) (obs label)))))

(defn posteriors
  "given datum, f: label -> log P(label | datum)"
  [params datum]
  (let [labels (all-labels params)
	ll (datum-log-prob params datum)
	log-sum (ss/log-add (map ll labels))]
    (mc/make-map #(Math/exp (- (ll %) log-sum)) labels)))
    	    
;;; -----------------
;;; Learn / Training
;;; -----------------

(defn- make-label-prior [data]
  (reduce 
    (fn [d [label datums]] (distr/obs d label (count datums)))    
    (distr/make-DirichletMultinomial :lambda 1.0)
    (group-by label data)))

(defn- make-feat-distrs 
  "returns a map feat => distr"
  [fvs]
  (reduce
   (fn [res [f fvs]] 
     (assoc res f 
	(apply
	  distr/obs-all 
	  (distr/make-DirichletMultinomial :lambda 1.0)
	  (mapcat (fn [[f v]] [v 1.0]) fvs))))       
   {} (group-by first fvs)))

(defn- feat-distrs-factory
  [data]
  (let [data-by-label (group-by label data)]
    (fn [label]
      (make-feat-distrs (mapcat feat-vec (data-by-label label))))))
     
(defn learn [data]
  (let [prior (make-label-prior data)]	
    (Params.
     (mc/make-map (feat-distrs-factory data) (distr/support prior))
     prior)))
	  
(comment
(def data [[:ham [[:word "sex"] [:head "spam"] [:word "viagra"]]]
           [:spam [[:head "spam"]]]])
(distr/support (make-label-prior data))
(def params (learn data))
(make-label-prior data)
(posteriors params (first data))
(all-labels params)
(posteriors params (first data))
)
