(ns mochi.nlp.summarization)

(defprotocol IDoc
  (sents [d] "sentences of document"))

(extend-protocol IDoc
  clojure.lang.IPersistentVector
  (sents [d]  d))

(defprotocol ISent
  (words [s] "words"))

(extend-protocol ISent
  clojure.lang.IPersistentVector
  (words [d]  d))

(defprotocol ISentExtractorSummarizer
  (summarize [this docs num-sents] "returns [doc-index sent-index]"))
