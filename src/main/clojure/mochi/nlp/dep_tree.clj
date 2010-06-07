(ns mochi.nlp.dep-tree  
  (:require [mochi [tree :as tree]]
	    [mochi.nlp [ling-tree :as lt]])
  (:use [mochi core]))

(defn- to-stanford-tree
  { :doc "Make Stanford Tree"
    :tag edu.stanford.nlp.trees.Tree }
  [t]  
  (-> (str t)
      (java.io.StringReader.)
      (edu.stanford.nlp.trees.PennTreeReader. (edu.stanford.nlp.trees.LabeledScoredTreeFactory.))
      .readTree))

(defn- read-index-from-stanford-label
  [label]
  (-> (doto (re-matcher #".*-(\d+)$" label) (.matches))
      (.group 1)
      (Integer/parseInt)
      dec))

(defn to-typed-deps
  [t]
  (map
   (fn [dep] [(read-index-from-stanford-label (str (.gov dep)))
	      (read-index-from-stanford-label (str (.dep dep)))
	      (-> dep .reln .getShortName)])
   ; Gets Typed Dependencies
   (-> (to-stanford-tree t)
       edu.stanford.nlp.trees.EnglishGrammaticalStructure.
       .typedDependencies)))


(defn to-dep-tree
  {:doc "Convert tree to dependency representation.
   t: implements mochi.ITree and assumes it has
   had mochi.ling-tree/add-head-info called on it.
   If not, calls it on tree.
   Returns map from head index to seq of children"
   :author "aria42"}
  [t]
  (letfn [(head-index [n] (nth (:head-info n) 2))]
    (reduce
     (fn [res node]
       (let [i (head-index node)]
	 (if i
	   (update-in res [(head-index node)] 
              concat (filter identity (map head-index (tree/children node))))
	   res)))
     {}
     (tree/nodes (if (:head-info t) t (lt/add-head-info t))))))

(comment
  (defn head-index [n] (nth (:head-info n) 2))
  (def t
       (-> "(S (NP (DT the) (NN man)) (VP (VBD ran)))"
	   lt/read-tree
	   tree/add-ids
	   lt/add-head-info))
  (to-stanford-tree (str t))
  (head-index t)
  (def c (first (tree/children t)))
  (tree/pre-leaf? c)
  (map head-index (tree/nodes t))
  (to-dep-tree t)
  (to-typed-deps t)
)
  
