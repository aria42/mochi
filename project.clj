(defproject nlputil-clj "1.1-SNAPSHOT"
  :dependencies 
  [[org.clojars.pjt/opennlp-tools "1.4.3"]
    [org.clojure/clojure "1.2.0"]
    [org.clojure/clojure-contrib "1.2.0"]
    [clj-sys/plumbing "0.1.5-SNAPSHOT"]]
  :aot [mochi.nlp.process.tokenizer]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :repositories  {"apache" "https://repository.apache.org/content/repositories/releases/"})
   