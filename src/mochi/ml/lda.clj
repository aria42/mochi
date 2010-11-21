(ns mochi.ml.lda
  {:doc "Latent Dirichlet Allocation (LDA) using variational bayes.
   Only optimizing global topic distributions, not
   topic prior hyper-distributions."
   :author "Aria Haghighi <me@aria42.com>"}
  (:use [mochi.core :only [make-map]]
        [mochi.sloppy-math :only [digamma]])
  (:require [mochi.ml.distribution :as distr]
	    [mochi.counter :as cntr]))

(def *globals*
     {:vocab-lambda 1.0
      :topic-lambda 1.0
      :K 100
      :random (java.util.Random.)
      :num-iters 10
      :num-inner-esteps 10
      :doc-topic-lambda 1.0})

(defrecord TopicStats [word-counts]
  mochi.ml.distribution.ISuffStats
  (obs [this word weight]
       (TopicStats. (cntr/inc-count word-counts word weight)))
  (merge-stats [this other]
       (TopicStats. (cntr/merge-counters word-counts (:word-counts other))))
  (to-distribution [this]
       (distr/make-DirichletMultinomial
	:counts word-counts
	:lambda (-> *globals* :vocab-lambda)
	:numKeys (count word-counts))))

(defrecord DocStats [topic-counts]
  mochi.ml.distribution.ISuffStats
  (obs [this topic weight]
       (DocStats. (cntr/inc-count topic-counts topic weight)))
  (merge-stats [this other]
       (TopicStats. (cntr/merge-counters topic-counts (:topic-counts other))))
  (to-distribution [this]
     (let [logZ (digamma (cntr/total-count topic-counts))]
       (->>  (range (:K *globals*))
	     (make-map
	       (fn [topic]
		 (digamma (+ (topic-counts topic) (:topic-lambda *globals*)))))
	     (cntr/log-scores-to-probs)
	     second
	     cntr/all-counts))))

(defn new-stats []
  [(DocStats. {})
   (make-map (constantly (TopicStats. {})) (range (:K *globals*)))])

(defn word-posterior [word doc-topic-distr topic-vocab-distrs]
  (if (nil? topic-vocab-distrs)
    (->> (range (:K *globals*))
	 (make-map (fn [_] (-> *globals* :random .nextDouble)))
	 cntr/make
	 cntr/normalize
	 cntr/all-counts)
    (->> (range (:K *globals*))
	 (make-map 
	  (fn [t]
	    (+ (distr/log-prob doc-topic-distr t)
	       (distr/log-prob
		(topic-vocab-distrs t)
		word))))
	 cntr/log-scores-to-probs
	 second
	 cntr/all-counts)))

(defn doc-estep [doc doc-topic-distr topic-vocab-distrs]
  (reduce
   (fn [[doc-stats topic-word-stats] word]
     (let [posts (word-posterior word doc-topic-distr topic-vocab-distrs)]
       [(reduce
	   (fn [res [k v]] (distr/obs res k v))
	   doc-stats
	   posts)
	(reduce
	 (fn [res [t post]]
	   (update-in res [t]
		      distr/obs word post))
	 topic-word-stats
	 posts)]))   
   (new-stats)
   doc))

(defn init-doc-topic-distr []
  (make-map
   (constantly (/ 1.0 (:K *globals*)))
   (range (:K *globals*))))

(defn init-topic-vocab-stats []
  (into {}
    (for [t (:K *globals*)]
      [t (TopicStats. {})])))

(defn doc-inf [doc topic-vocab-distrs]
  (loop [iter 0
	 doc-topic-distr (init-doc-topic-distr)]
    (let [[new-doc-stats new-vocab-stats]
	  (doc-estep doc doc-topic-distr topic-vocab-distrs)]
      (if (= iter (:num-inner-esteps *globals*))
	[new-doc-stats new-vocab-stats]
	(recur (inc iter)
	       (distr/to-distribution new-doc-stats))))))

(defn doc-topics [topic-vocab-distrs doc]
     (-> (doc-inf doc topic-vocab-distrs)
	 first
	 distr/to-distribution))

(defn global-iter [docs topic-vocab-distrs]
  (->> docs
       (pmap (fn [doc] (second (doc-inf doc topic-vocab-distrs))))
       (reduce (partial merge-with distr/merge-stats))
       (pmap
	(fn [[topic stats]] [topic (distr/to-distribution stats)]))
       (into {})))

(defn learn [docs]
  (loop [iter 0 topic-vocb-distrs nil]
    (if (= iter (:num-iters *globals*))
        topic-vocb-distrs
	(recur
	 (inc iter)
	 (global-iter docs topic-vocb-distrs)))))
