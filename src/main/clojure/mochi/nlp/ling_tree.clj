(ns mochi.nlp.ling-tree
  (:require [mochi [span :as span]]
	    [mochi.nlp [collins-head-rules :as collins]])
  (:use [mochi tree])
  (:import [mochi.tree Tree]))

;;; --------------------------
;;; Ling Trees with head info
;;; --------------------------

(defn add-head-info
 "t: tree with string label
  hf: head finder takes node returns head index
  returns tree with :head-info [head-word head-tag head-index] info
  at each node"
  ([t hf]
     (if (pre-leaf? t)
       (let [tag (:label t)
	     word (-> t children first :label)
	     index (span/start t)]
	 (assoc t :head-info [word tag index]))
       (let [new-children (map #(add-head-info % hf) (children t))]
	 (into t
	       {:children new-children
		:head-info (:head-info (nth new-children (hf t)))}))))
  ([t] (add-head-info t collins/find-head-child)))

;;; -------------------------------------------
;;; PennTree Reader
;;; -------------------------------------------

(defn- is-label-char [#^Character c]
  (not (or (Character/isWhitespace c) (#{\( \)}  c))))

(defn- label-from-str [s]
  (let [[taked dropped] (split-with is-label-char s)]        
    [(apply str taked) dropped]))

(declare tree-from-str)

(defn- node-from-str [s]
  (assert (= (first s) \())
  (let [[label remain] (label-from-str (rest s))]    
    (loop [children [] remain remain]
      (if (= (first remain) \))
        [(Tree. label children) (rest remain)]
        (let [[node remain] (tree-from-str remain)]
          (recur (conj children node) remain))))))

(defn- trim-init-space [s] (drop-while #(Character/isWhitespace #^Character %) s))

(defn tree-from-str [s]
  (let [s (trim-init-space s)]
    (if (empty? s) nil
	(try
	 (if (= (first s) \()
	   (node-from-str s)
	   (let [[label rest] (label-from-str  s)]
	     [(Tree. label []) rest]))
	 (catch Exception e
	   (throw (RuntimeException. (str "Error parsing tree from: " (str e)))))))))

(defn read-trees
  "Read sequence of trees from string"
  [s]
  (if (not (empty? s))
    (let [[tree s] (tree-from-str s)]
      (conj (read-trees (trim-init-space s)) tree))
    []))

(defn read-tree
  "Read (first) tree from string"
  [s] (first (read-trees s)))

(comment
  (str (read-tree "(NP (DT the) (NN man))"))
)
