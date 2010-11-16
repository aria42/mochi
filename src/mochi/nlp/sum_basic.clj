(ns mochi.nlp.sum-basic
  (:use [mochi core]
	[mochi.nlp summarization])
  (:require 
     [clojure.contrib.seq-utils :as su]   
     [mochi.counter :as cntr]))

(defn- init-probs [docs] 
  (->> (for [d docs  s (sents d) w (words s)] w)
       (reduce 
	       (fn [cs w] (cntr/inc-count cs w 1)) 
	       (transient (cntr/make)))
       persistent!
       cntr/normalize))

(defn avg-prob 
  "average unigram prob"
  [probs sent]
  (/ (reduce + (map probs (words sent)))
     (count (words sent))))

(defn- select-sent [f docs so-far]
  (let [[dix six sent]
         (apply max-key 
             (fn [[dix six sent]] (f sent))
      	     (for [[dix d] (su/indexed docs)
      		         [six s] (su/indexed (sents d))
      		         :when (not (so-far [dix six]))] [dix six s]))]
    [dix six]))

(defn sq-prob-update [probs sent]
  (->>  (into #{} (words sent))	
	(reduce
	 (fn [probs w] 
	   (let [p (probs w)]
	     (cntr/set-count probs w (* p p))))
	 probs)
	cntr/normalize))

(deftype SumBasic [sent-score-fn prob-update-fn] 
  ISentExtractorSummarizer
  (summarize
   [this docs num-sents]
   (loop [summr #{} probs (init-probs docs)]
     (if (< (count summr) num-sents)
       (let [[dix six] (select-sent (partial sent-score-fn probs) docs summr)]
	 (recur (conj summr [dix six]) 
		(prob-update-fn probs (-> docs  (nth dix) sents (nth six)))))
       summr))))

(defn sum-basic-summarize
  "each element of docs is a seq of sentences,
   each sentence is a seq of word strings.

   Returns seq of [doc-index sent-index] of the
   sentences to use"
  [docs num-sents]
  (summarize (SumBasic. avg-prob sq-prob-update) docs num-sents))

(comment
  (def docs [[[:b :c :d]]
	     [[:a :b :c]]
	     [[:b :c] [:d :f]]
	     [[:d :e]]])
  (sum-basic-summarize docs 2)
)
  

