(ns mochi.tree
  (:use [mochi core seq-utils vec-utils]    
	[clojure.contrib str-utils])
  (:require [mochi [span :as span] [tree :as tree]]))

;;; -------------------------------------------
;;; ITree
;;; -------------------------------------------

(defprotocol ITree
  (label [t] "label data at node")
  (children [t] "children at node"))

(extend-protocol ITree

   clojure.lang.IPersistentVector
   ; Vectors are interepted as trees
   ; e.g. ["NP" ["DT" "the"] ["NN" "man"]]
   (label [t] (first t))
   (children [t] (if-let [cs (rest t)] cs []))
   
   String
   ; Every String can be treated as the leaf of a tree
   (label [t] t)
   (children [t] nil))


;;; -------------------------------------------
;;; ITree Methods
;;; -------------------------------------------

(defn leaf? [t]
  (empty? (children t)))

(defn pre-leaf? [t]
  (let [cs (children t)]
    (and (singleton? cs)
	 (leaf? (first cs)))))

(defn nodes 
  "all nodes of the tree in a pre-order walk"
  ([t pre-order?]
    (if pre-order? 
      (cons t (mapcat nodes (children t)))
      (conj (ensure-vec (mapcat nodes (children t))) t)))
  ([t] (nodes t true)))

(defn leaves [t]
  (filter leaf? (nodes t)))

(defn pre-leaves [t]
  (filter pre-leaf? (nodes t)))

(defn yield [t]
  (map label (leaves t)))

(defn pre-yield [t] 
  (map label (pre-leaves t)))

;;; -------------------------------------------
;;; Tree Data Type
;;; This supports much more than ITree.
;;; Each label is a map
;;; -------------------------------------------

(defrecord Tree [_label _children]

    ITree
    (label [t] _label)
    (children [t] _children)

    Object
    (toString [this]
      (if (leaf? this)
       (str _label)
       (format "(%s %s)" (str _label) (str-join " " (children this)))))

    span/ISpan
    (start [this] (first (:id this)))
    (stop [this] (second (:id this))))

(defn depth [t]
  (if-let [id (:id t)] 
    (nth id 2)
    (if (leaf? t) 0
      (inc (apply max (map depth (children t)))))))

(defn add-ids 
  "Returns tree which adds  :id -> [start stop depth] to each node's map. This
   triple uniquely identifies a node in a tree."
  ([t start]     
     (if (leaf? t)      
       (assoc (Tree. (label t) []) 
	      :id [start (inc start) (depth t)])
       (let [span-fn 
	     (fn [[cs offset] c] 
	       (let [new-child  (add-ids c offset)]
		 [(conj! cs new-child) (second (:id new-child))]))
	     [new-children end]  
	      (reduce span-fn [(transient []) start] (children t))]
	 (assoc
	   (Tree. (label t)  (persistent! new-children))
	   :id [start end (depth t)]))))
  ([t] (add-ids t 0)))

;;; -------------------------------------------
;;; Tree Factory Methods
;;; -------------------------------------------

(defn make
  ([label children] (add-ids (Tree. label children)))
  ([label] (make label [])))

(defn to-tree [t]
  (if (vector? t)
    (make (first t) (vec (map to-tree (rest t))))
    (make t)))

;;; -------------------------------------------
;;; Make New Trees
;;; -------------------------------------------
		     			         
(defn transform [t label-fn]
  (Tree. (label-fn t) (map #(transform % label-fn) (children t))))

;;; -------------------------------------------
;;; Path Functions: Require Tree Datatype
;;; -------------------------------------------

(defn find-lca
  "find lowest common ancestor
  t: supports ITree and ISpan (Tree does this)
  n1, n2: are nodes in t"
  [t n1 n2] 
  (when (and (span/contains? t n1)  (span/contains? t n2))
    (let [res (first (filter identity (map #(find-lca % n1 n2) (children t))))]
      (or-else res t))))

(defn path-to
  "find path between two nodes where one is an ancestor;
  if one is not an ancestor, you will get the empty seq"
  [from to]
  (if (= from to) []
      (let [new-from 
	    (first (for [c (children from)	    	     
			 :when (span/contains? c to)] c))]
	(cons from (path-to new-from to)))))

(defn find-path
  [t start stop]
  (let [lca (find-lca t start stop)]
    (concat (reverse (path-to lca start)) (rest (path-to lca stop)))))

(comment  
 (def t (to-tree  ["NP" ["NP" ["DT" "The"] ["NN" "man"]] ["NP" ["DT" "here"]]]))
 (label t)
 (children t)
 (str t)
 (println t)
 (str (to-tree t))
 (def ls (leaves t))
 (map str ls)
 (map label (path-to t (first ls)))
 (map label (find-path t (first ls) (last ls)))
 (yield (find-lca t (first ls) (nth ls 2)))
)
  
