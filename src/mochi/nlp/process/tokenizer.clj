(ns mochi.nlp.process.tokenizer
  (:require [clojure.contrib [seq-utils :as cc.su]])
  (:use [clojure.contrib singleton def]   
	[mochi core vec-utils file-utils]))

(defprotocol ISpanTokenizer
  (token-spans [this txt] "only returns char spans of tokens"))

(defrecord Token [word raw-word char-span source])

(defn span-tokenize [tokenizer txt]    
  { :pre [(satisfies? ISpanTokenizer tokenizer)] }  
  (ensure-vec  
   (map (fn [[start stop]]
	  (let [word (.substring txt start stop)]
	    (Token. word word [start stop] txt)))
       (token-spans tokenizer txt))))

(defrecord OpenNLPSpanTokenizer [#^opennlp.tools.lang.english.Tokenizer toker-impl]
  ISpanTokenizer
  (token-spans [this txt] 
     (for [s (.tokenizePos toker-impl txt)] [(.getStart s) (.getEnd s)])))

(defn- split-spans [[start stop] txt]
  (let [w (.substring txt start stop)]
    (cond
     (and (> (.length w) 1) (.startsWith w "\""))
       [[start (inc start)] [(inc start) stop]]
     (and (> (.length w) 1) (.endsWith w "\""))
       [[start (dec stop)] [(dec stop) stop]]
     (.contains w "--")
       (let [ix (+ (.indexOf w "--") start)]
	 [[start ix] [ix (+ ix 2)] [(+ ix 2) stop]])
     :default [[start stop]])))   
      
(defrecord SplitSpanTokenizer [base-toker-impl]
  ISpanTokenizer
  (token-spans [this txt]
    (filter
      (fn [[start stop]] (> stop start))
      (mapcat #(split-spans % txt) (token-spans base-toker-impl txt)))))

(def open-nlp-tokenizer
  (per-thread-singleton 
    #(-> "opennlp_models/EnglishTok.bin.gz"
	    (resource-to-temp-file  ".bin.gz")
	    (.getAbsolutePath)
	    (opennlp.tools.lang.english.Tokenizer.)
	    (OpenNLPSpanTokenizer.)
	    (SplitSpanTokenizer.))))

(defn- quote-post-fix [toks]
  (for [[i tok] (cc.su/indexed toks)]
    (if-not (= (:raw-word tok) "\"") tok
      (let [before (when (> i 0) (toks (dec i)))
	    after  (when (< (inc i) (count toks)) (toks (inc i)))]
	(cond
	 ;; Touches Previous Token: Closed Quote
	 (and before (= (-> before :char-span second) (-> tok :char-span first)))
	 (assoc tok :word "''")
	 ;; Touches Next Token: Open Quote
	 (and after (= (-> after :char-span first) (-> tok :char-span second)))
	 (assoc tok :word "``")
	 ;; Default
	 :default (throw (RuntimeException. "Isolated Quote")))))))
	 
	      
(defn- ptb-post-fix [toks]
  (for [t (quote-post-fix toks) :let [w (:word t)]]
    (cond 
     (= "(" w) (assoc t :word "-LRB-")
     (= ")" w) (assoc t :word "-RRB-")
     :default t)))

(defn tokenize 
  "Returns seq of Token objects"
  ([txt] (tokenize (open-nlp-tokenizer) txt))
  ([toker-impl txt] (ptb-post-fix (span-tokenize toker-impl txt))))

(comment
  (System/getProperties)
  (ClassLoader/getSystemResourceAsStream "opennlp_models/EnglishTok.bin.gz") 
  (open-nlp-tokenizer)
  (tokenize "Aria is cool.")
)

  