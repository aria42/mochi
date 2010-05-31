(ns mochi.lda
  (:use [clojure.contrib seq-utils])
  (:require [mochi.cli :as cli]))

;;; -------------------
;;; Vocab
;;; -------------------

(defn makeVocabIndex
  "Make an indexing of vocab 
  docs: seq of seq of words
  returns: vec of unique words"
  [docs]
  (vec (into #{} (mapcat identity docs))))

;;; -------------------
;;; Corpus
;;; -------------------

(deftype Corpus 
  "docs: seq of [word index] pairs, where index is word-type index
   vocab: vec of words that serves as indexing"
  [docs vocab]
  clojure.lang.IPersistentMap)

(defn makeCorpus 
  "docs: seq of seq of words
   returns: Corpus "
  [docs]
  (let [vocab (makeVocabIndex docs)
        docs (let [vocab-map (into {} (for [[i w] (indexed vocab)] [w i]))]
               (for [doc docs] (vec (for [w doc] [w (vocab-map w)]))))]
    (Corpus docs vocab)))

;;; ---------------------
;;; Constants
;;; ---------------------

(def *K* 10)

;;; ---------------------
;;; Utility
;;; ---------------------

(defn prob-normalize 
  "xs: seq of doubles
   returns: vec of doubles, summing to 1.0"
  [xs]
  (let [sum-elems (reduce + xs)]    
    (into (vector-of :double) (map #(/ % sum-elems) xs))))

;;; -----------------------------------
;;; Variational Distribution
;;; beta: vocab parameters
;;; alphas: topic-hyper params
;;; gamma: document-level topic params
;;; phis: document-level indicator
;;; ------------------------------------

(defn compute-phi [betas gammas #^int w]
  (prob-normalize 
   (for [i (range *K*)]    
    (* (-> betas (nth i) (nth w))
       (-> i gammas (fig.basic.NumUtils/digamma) (Math/exp))))))

(defn compute-gammas [alphas phis]
  (into (vector-of :double) (map + alphas (apply map + phis))))

(defn variational-estep-iter [doc phis gammas alphas betas]
  (let [new-phis (for [[w i] doc] (compute-phi betas gammas w-i))]
    [new-phis (compute-gammas alphas new-phis)]))

(defn init-phis [N]
  (for [n (range N)] (for [k (range *K*)] (/ 1.0 *K*))))

(defn init-gammas [N alphas]
  (for [k (range *K*)] (+ (alphas k) (/ N *K*))))

(defn variational-estep [doc alphas betas num-iters]
  (let [N (count doc)]
    (last (take num-iters
      (iterate
        (fn [[phi gammas]] (variational-estep-iter doc phis gammas alphas betas))
        [(init-phis N) (init-gammas N alphas)])))))

(defn estep [corpus alphas betas]
  (reduce
    (fn [x y] (merge-with + x y))
    (map
      (fn [doc]
        (let [[phis _] (variational-estep doc alphas betas 5)]
	  (for [[t row]  phis]
	    (into {} (map (fn [[w i]]  [w ]) doc))))
      (:docs corpus)))))

(defn mstep [sum-phis]
  (vec (map prob-normalize sum-phis)))

(defn init-betas [V]
  (let [r (java.util.Random. 0.0)]
    (for [k *K*] (prob-normalize (for [w V] (.nextDouble r))))))

(defn lda [corpus alpha-val num-iters]
  (let [V (count (:vocab corpus)) alphas (for [k *K*] alpha-val)]
    (last (take num-iters (iterate #(mstep (estep corpus alphas %)) (init-betas V))))))

;;; -----------------
;;; Main
;;; -----------------

(def docs [[:a :b :a], [:b :c :a]])
(lda (makeCorpus docs) 1.0 2)