(ns mochi.nlp.collins-head-rules
  (:use mochi.core)
  (:require [clojure.contrib [seq-utils :as su]]
            [mochi [tree :as tree]])) 
 
 
(defn- abstract-rule 
  "Abstract Collins Head Rule. Returns index of head child.
   - t: Supports ITree protocol (includes Berkeley Trees)
   - spec: composed of [[dir dis?] cats], where
   dir is in {:left,:right} and dis? is boolean. cats are
   seq of non-term categories. If dis?, search for first cat you
   can find looking left to right if :left, right to left otherwise.
   Else, search in dir for first child matching any of cats.
   NOTE: for loop is lazy, so only generates at most 1 satisfying element"
  [t spec]
  (let [ [[dir dis?] cats] spec
	 dir-fn (if (= dir :left) identity reverse)
	 nodes  (-> t tree/children su/indexed dir-fn)
	 matches? (fn [n c] (.equals #^String (tree/label n) c)) ]
     (first
       (if dis?
         (for [[i n] nodes, c cats :when (matches? n c)] i)	    
         (for [c cats, [i n] nodes :when (matches? n c)] i)))))      

(def +rules+
  { :ADJP [[[:left false] 
	    ["NNS", "QP", "NN","$", "ADVP", "JJ", "VBN", "VBG", "ADJP", "JJR", 
	     "NP", "JJS", "DT","FW", "RBR", "RBS", "SBAR", "RB"]]]
   :ADVP  [[[:right false]
	   ["RB", "RBR", "RBS","FW", "ADVP", "TO", "CD", 
	   "JJR", "JJ", "IN", "NP", "JJS", "NN"]]]
   :CONJP [[[:right false] ["CC", "RB", "IN"]]]
   :FRAG  [[[:right false]]]   
   :INTJ  [[[:left false]]]
   :LST   [[[:right false] ["LS" ":"]]]
   :NAC   [[[:left false] 
	    ["NN" "NNS" "NNP" "NNPS" "NAC" "EX", "$", "CD", "QP",
	     "PRP", "VBG", "JJ", "JJS", "JJR", "ADJP", "FW"]]]
   :NX    [[[:left false]]]
   :PP    [[[:right false] ["IN" "TO" "VBG" "VBN" "RP" "FW"]]]
   :PRN   [[[:left false]]]
   :PRT   [[[:right false] ["RP"]]]
   :QP    [[[:left false] 
	    ["$" "IN" "NNS" "NN" "JJ" "RB" "DT" "CD" "NCD" "QP" "JJR" "JJS"]]]
   :RRC   [[[:right false] ["VP" "NP" "ADVP" "ADJP" "PP"]]]
   :S     [[[:left false] ["TO" "IN" "VP" "S" "SBAR" "ADJP" "UCP" "NP"]]]
   :SBAR  [[[:left false] 
	    ["WHNP" "WHPP" "WHADVP" "WHADJP" "IN" "DT" "S" "SQ" "SBAR" "FRAG"]]]
   :SBARQ [[[:left false] ["SQ" "S" "SINV" "SBARQ" "FRAG"]]]
   :SINV  [[[:left false] ["VBZ" "VBD" "VBP" "VB" "MD" "VP" "S" "SINV" "ADJP" "NP"]]]
   :SQ    [[[:left false] "VBZ", "VBD", "VBP","VB", "MD", "VP", "SQ" ]]			
   :UCP    [[[:right false]]]
   :VP     [[[:left false] 
	     ["TO" "VBD" "VBN" "MD" "VBZ" "VB" "VBG" "VBP" "AUX"
	       "AUXG" "VP" "ADJP" "NN" "NNS" "NP"]]]	    
   :WHADJP [[[:right false] ["CC" "WRB" "JJ" "ADJP"]]]
   :WHADVP [[[:right false] ["CC" "WRB"]]]
   :WHNP  [[[:left false] ["WDT", "WP", "WP$","WHADJP", "WHPP", "WHNP"]]] 
   :WHPP  [[[:right false] ["IN" "TO" "FW"]]]
   :X     [[[:right false]]]
   :NP    [[[:right true] ["NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR"]]	    
	   [[:left false] ["NP"]]
	   [[:right false] ["CD"]]
	   [[:right true] ["JJ" "JJS" "RB" "QP"]]]   
  })
	   
(defn find-head-child
  "find head daughter index given rules"
  ([t rules]  
     (or-else 
       (some (partial abstract-rule t) (-> t tree/label keyword rules))	    
       0))
  ([t] (find-head-child t +rules+)))

(defn find-head-word
  "recursively finds head child until it returns pre terminal node
   t: supports ITree 
   hf (head-finder): tree -fn-> child index"
  [t hf]
  (first (filter tree/pre-leaf?    
		 (iterate #(nth (tree/children %) (hf %)) t))))


(comment
  (def t  ["S" ["NP" ["DT" "the"] ["JJ" "quick"] ["JJ" "brown"] ["NN" "fox"]]	  
	      ["VP" ["VBD" "jumped"] 
	       ["PP" ["IN" "over"] 
		["NP" ["DT" "the"] ["JJ" "lazy"] ["NN" "dog"]]]]])
  (str (find-head-word (tree/to-tree t) find-head-child))
)  
