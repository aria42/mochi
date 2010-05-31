(ns mochi.test-counter  
  (:require [mochi.tree :as tree]
	    [mochi.nlp.ling-tree :as ltree]
	    [mochi.span :as sp]
	    [mochi.counter :as cntr]
	    [clojure.contrib.seq-utils :as su])
  (:use [mochi core]
        [clojure.contrib duck-streams]))

(defn lines 
  "get lines from gz file"
  [#^String path]
  (-> path
      java.io.FileInputStream.
      java.util.zip.GZIPInputStream.
      java.io.InputStreamReader.
      java.io.BufferedReader.
      line-seq))

;; (defn words
;;   [path]
;;   (for [l (lines path) w (re-seq #"\w+" l)] w))

;; (time 
;;   (do 
;;     (def c 
;; 	 (reduce
;; 	  (fn [cs w] (cntr/inc-count cs w 1.0))
;; 	  (to-transient (cntr/ICounter))
;; 	  (words "/usr/local/corpora//NANC/003.gz")))
;;     (def c (to-persistent! c))
;;     (println (cntr/total-count c))
;;     (println (c "the"))))

;; (time
;;  (do
;;    (def c (edu.berkeley.nlp.util.ICounter.))
;;    (doseq [w (words "/usr/local/corpora//NANC/003.gz") ]
;;      (.incrementCount #^edu.berkeley.nlp.util.ICounter  c w 1.0))
;;    (println (.totalCount c))
;;    (println (.getCount c "the"))))

;;(println (take 10 (words "/usr/local/corpora//NANC/003.gz")))

(defn trees [path] 
  (for [l (lines path) 
	:when (not (empty? l))]
    (-> l ltree/read-tree ltree/add-head-info)))

(defn all-words [path] (mapcat tree/yield (trees path)))

;(println (take 10 (all-words "/usr/local/corpora//NANC/003.gz")))

;; (def cs
;;   (to-persistent!
;;    (reduce
;;     (fn [cs #^String w] (cntr/inc-count cs (.toLowerCase w) 1.0))
;;     (to-transient (cntr/make))
;;     (mapcat tree/yield (trees "/usr/local/corpora//NANC/003.gz")))))

;(println (str cs))

;; (defn base-np? [t]
;;   (and (= (.label t) "NP")
;;        (> (-> t .children .size) 1)
;;        (every? tree/pre-leaf? (.children t))))

;; (doseq [t (trees "/usr/local/corpora//NANC/003.gz")
;;         n (tree/nodes t) :when (base-np? n)]
;;   (println (str n) " -> " (:head-info n)))
