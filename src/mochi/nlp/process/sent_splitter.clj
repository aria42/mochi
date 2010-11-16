(ns mochi.nlp.process.sent-splitter
  (:import [opennlp.tools.lang.english SentenceDetector])
  (:use [clojure.contrib singleton def repl-utils]
	[clojure.contrib.javadoc browse]	
   [mochi core file-utils]
   [mochi.nlp.process tokenizer]))

(def- open-nlp-sent-split
     (per-thread-singleton
      #(-> "opennlp_models/eng-sent-seg.bin.gz"
	   (resource-to-temp-file  ".bin.gz")
	   (.getAbsolutePath)
	   (opennlp.tools.lang.english.SentenceDetector.))))

(defrecord Sentence [toks char-span source])

(defn make-Sentence [toks char-span source]
  (Sentence. toks char-span source))

(defn sent-spans
  "return vec of [start stop] indices for sentence text"
  [txt] 
  (partition 2 1 
    (concat [0] (.sentPosDetect (open-nlp-sent-split) txt) [(.length txt)])))

(defn split-sents 
  "return vec of sentences"
  [txt] 
  (map 
   (fn [[start stop]]
     (let [sent-txt (.substring txt start stop)]	   
       (Sentence. (tokenize sent-txt) [start stop] txt)))
   (sent-spans txt)))

