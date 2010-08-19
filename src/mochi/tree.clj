(ns mochi.tree
  (:use [mochi core seq-utils vec-utils]
    [clojure.contrib str-utils])
  (:require [mochi [span :as span] [tree :as tree]]))


;;; ITree ;;;

(defprotocol ITree
  (label [t] "label data at node")
  (children [t] "children at node"))

(extend-protocol ITree

  clojure.lang.IPersistentVector
  ; Nested vectors are interepted as trees
  ; e.g. ["NP" ["DT" "the"] ["NN" "man"]]
  (label [t] (first t))
  (children [t] (if-let [cs (rest t)] cs []))

  String
  ; Every String can be treated as the leaf of a tree
  (label [t] t)
  (children [t] nil))

;;; ITree Methods ;;;

(defn leaf?
  "does this node have any children?"
  [t]
  (empty? (children t)))

(defn pre-leaf?
  "does this node have only a leaf child"
  [t]
  (let [cs (children t)]
    (and (singleton? cs)
      (leaf? (first cs)))))

(defn nodes
  "all nodes of the tree in a pre-order walk"
  ([t]
    (loop [queue [t] res []]
      (if-let [n (first queue)]
        (recur
          (concat (children n) (rest queue))
          (conj res n))
        res))))

(defn leaves [t]
  (filter leaf? (nodes t)))

(defn pre-leaves [t]
  (filter pre-leaf? (nodes t)))

(defn yield
  "the label of each leaf node"
  [t]
  (map label (leaves t)))

(defn pre-yield
  "the label of each pre-leaf node"
  [t]
  (map label (pre-leaves t)))

;;; Tree Data Type ;;;
;;; This supports much more than ITree.
;;; Each node is also a map to store more data

(defrecord Tree [_label _children]

  ITree
  (label [t] _label)
  (children [t] _children)

  Object
  (toString [this]
    (if (leaf? this)
      (str _label)
      (format "(%s %s)" (str _label) (str-join " " _children))))

  span/ISpan
  ; Each tree knows its span if you've added ids (see add-ids)
  (start [this] (first (:id this)))
  (stop [this] (second (:id this))))

(defn depth [t]
  (if-let [id (:id t)]
    (last id)
    (if (leaf? t) 0
      (inc (apply max (map depth (children t)))))))

(defn add-ids
  "Returns tree which adds  :id -> [start stop depth] to each node's map. This
triple uniquely identifies a node in a tree. Note that this means
each node is now grounded in a paritcular tree and you can't meaningfully
share tree structure."
  ([t start]
    (if (leaf? t)
      (assoc (Tree. (label t) [])
        :id [start (inc start) 0])
      (let [span-fn
	     (fn [[cs offset] c]
	       (let [new-child (add-ids c offset)]
		 [(conj! cs new-child) (second (:id new-child))]))
	     [new-children end]
	     (reduce span-fn [(transient []) start] (children t))]
        (assoc
          (Tree. (label t) (persistent! new-children))
          :id [start end (depth t)]))))
  ([t] (add-ids t 0)))

;;; Tree Factory Methods ;;;

(defn make
  "Returns a Tree record and does add-ids so each
  node knows the dominating span and depth. You
  cannot share tree structure. "
  ([label children] (add-ids (Tree. label children)))
  ([label] (make label [])))

(defn to-tree [t]
  (if (vector? t)
    (make (first t) (vec (map to-tree (rest t))))
    (make t)))

;;; Make New Trees ;;;

(defn transform [t label-fn]
  (Tree. (label-fn t) (map #(transform % label-fn) (children t))))

;;; Path Functions: Require Tree Datatype ;;;

(defn find-lca
  "find lowest common ancestor
  t: supports ITree and ISpan (Tree does this)
  n1, n2: are nodes in t"
  [t n1 n2]
  (when (and (span/contains? t n1) (span/contains? t n2))
    (let [res (first (filter identity (map #(find-lca % n1 n2) (children t))))]
      (or-else res t))))

(defn- path-to
  "find path between two nodes where one is an ancestor;
  if one is not an ancestor, you will get the empty seq"
  [from to]
  (if (= from to) []
    (let [new-from
	   (first (for [c (children from)
			 :when (span/contains? c to)] c))]
      (cons from (path-to new-from to)))))

(defn find-path
  "find path from start to stop in tree"
  [t start stop]
  (let [lca (find-lca t start stop)]
    (concat (reverse (path-to lca start)) (rest (path-to lca stop)))))

(comment
  (ns mochi.tree)
  (def t (to-tree ["NP" ["NP" ["DT" "The"] ["NN" "man"]] ["NP" ["DT" "here"]]]))
  (nodes t)
  (label t) 
  (str t)
  (println (str t))
  (str (to-tree t))
  (def ls (leaves t))
  (map str ls)
  (map label (find-path t (first ls) (last ls)))
  (yield (find-lca t (first ls) (last ls)))
  )

