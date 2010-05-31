(ns mochi.nlp.process.parser
  (:import [edu.berkeley.nlp.PCFGLA Parser
	    Grammar Lexicon ParserData CoarseToFineMaxRuleParser TreeAnnotations]
	   [edu.berkeley.nlp.syntax Tree]
	   [edu.berkeley.nlp.util Numberer])
  (:require [mochi [tree :as tree] [logger :as logger]])
  (:use [clojure.contrib singleton]
	[mochi core]))

(defn #^ParserData load-pdata []
  (logger/track "loading-pdata"
     (-> "berkeley_models/eng_sm5.gr.gz" 
	 ClassLoader/getSystemResource .getPath ParserData/Load)))

(def pdata (global-singleton 
  #(doto (load-pdata)
     (-> .getNumbs Numberer/setNumberers))))

(def berkeley-parser (per-thread-singleton     
   (fn [] (CoarseToFineMaxRuleParser. (.getGrammar #^ParserData (pdata)) (.getLexicon #^ParserData  (pdata))
      1.0 -1 false false false false false true true))))

(defn- berkeley-to-mochi-tree [#^Tree t]
  (tree/make (.getLabel t) 
	     (vec (map berkeley-to-mochi-tree (.getChildren t)))))

(defn parse-sent 
  "return a mochi.tree.Tree for seq of words (all seqs implement java.util.List).
   Like everything else, this function is concurrency safe."
  [& words]
  (-> #^CoarseToFineMaxRuleParser (berkeley-parser)
      (.getBestParse (to-jlist words))
      TreeAnnotations/unAnnotateTree
      berkeley-to-mochi-tree))
  
;(comment
;  (println (str (parse-sent "Aria" "is" "cool" ".")))
;)
