(ns mochi.nlp.ling-tree
  (:require [clojure [string :as string]]
            [mochi [span :as span] [tree :as tree]]
            [mochi.nlp [collins-head-rules :as collins]]))

;;; --------------------------
;;; Ling Trees with head info
;;; --------------------------

(defn add-head-info
 "t: tree with string label
  hf: head finder takes node returns head index
  returns tree with :head-info [head-word head-tag head-index] info
  at each node"
  ([t hf]
     (if (tree/pre-leaf? t)
       (let [tag (tree/label t)
	     word (-> t tree/children first tree/label)
	     index (span/start t)]
	 (assoc t :head-info [word tag index]))
       (let [new-children (map #(add-head-info % hf) (tree/children t))]
	 (-> (mochi.tree.Tree. (tree/label t) new-children)
	     (assoc :head-info (:head-info (nth new-children (hf t))))))))
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
        [(mochi.tree.Tree. label children) (rest remain)]
        (let [[node remain] (tree-from-str remain)]
          (recur (conj children node) remain))))))
          
(defn remove-punc [tree]
  (tree/filter-nodes tree 
    (fn [node]       
      (and (tree/pre-leaf? node) (#{"#" "$" "." "," "-LRB-" "-RRB-" "``" "''" ":"} (tree/label node))))
    false))          

(defn strip-empty [tree]
  (tree/filter-nodes tree
   (fn [node]
     (let [label (tree/label node)]
       (or (and (tree/leaf? node)  (re-matches #"-.*-" label))
           (and (tree/pre-leaf? node) (= label "-NONE-")))))
   false))

(defn standard-transform [tree]
  (let [tree (strip-empty tree)]
    (if (not (nil? tree))
       (->> tree
         (tree/map-label (fn [#^String  l] (if (-> l .trim empty?) "ROOT" l)))
         (tree/map-label (fn [l] (string/replace l #"(\w+)-.+$" second))))
       tree)))       

(defn- trim-init-space [s]
  (drop-while (fn [#^Character c] (Character/isWhitespace  c)) s))

(defn tree-from-str [s]
  (let [s (trim-init-space s)]
    (if (empty? s) nil
	(try
	 (if (= (first s) \()
	   (node-from-str s)
	   (let [[label rest] (label-from-str  s)]
	     [(mochi.tree.Tree. label []) rest]))
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
  [s] (first (tree-from-str s)))
