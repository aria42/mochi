(ns mochi.cli
  {:doc "Command-Line-Options Utility"
   :author "aria42" }
  (:use [mochi core])
  (:require [clojure.contrib [duck-streams :as ds]]))

(defn- process-file [f]
  (into {}
    (for [#^String l (-> f ds/reader line-seq) 
	  :let [[k v] (.split l "\\t")]]
      [(keyword k) v])))

(defn process-args [args]
  (loop [arg-map {} args args unassoc-args []]
    (if (empty? args) [arg-map unassoc-args]
      (let [#^String k (first args)]
	(cond
	 (.startsWith k "+")
	   (recur (into arg-map (-> k (.replaceAll "^[+]+", "") process-file)) 
		  (rest args) unassoc-args)
 	 (and (.startsWith k "-") (.endsWith k "?"))
	   (recur (assoc arg-map (keyword (.replaceAll k "^[-]+" "")) nil) 
		  (rest args) unassoc-args)
	 (.startsWith k "-")
	   (recur (assoc arg-map (keyword (.replaceAll k "^[-]+" "")) (second args)) 
		  (drop 2 args) unassoc-args)
	 :else 
	   (recur arg-map (rest args) (conj unassoc-args k)))))))

(defn- get-opt-val [opt-map spec]
  (let [[name,required?,doc,default] spec]
    (when (and required? (not (opt-map name)))
      (throw (RuntimeException. (format "Required option: %s, not set " name))))
    (if (instance? Callable default)
      (try (default (opt-map name))		       		       
	   (catch Exception e (throw (RuntimeException. 	      	     
	     (format "Couldn't process %s for opt %s" (opt-map name) name)))))
      (opt-map name default))))
    
(defn- build-opts [specs opt-map]
  (reduce 
    (fn [res spec] 
      (assoc res (first spec) (get-opt-val opt-map spec)))
    {}
    specs))

(defn do-help [specs]
  (doseq [[name,req?,doc,default] specs]
    (println (str name "\t" doc 
		  (when req? " [required]"))))
  (System/exit 0))


;;; -----------------
;;; Main
;;; -----------------

(defn parse-cmd-line 
  "parse a cmd-line
   specs: Each spec has the form [name,required?,doc-string,default-val-of-fn]
   Returns a map from keyword of spec-name to value processed by default-fn 
   or with default val. Will print specs if -h or -help are passed"
  [args & specs] 
  (let [[opt-map args] (process-args args)]
    (when (or (contains? opt-map "h") (contains? opt-map "help"))
      (do-help specs))
    [(build-opts specs opt-map) args]))

(defn load-options
  [& specs]
  (parse-cmd-line *command-line-args* specs))
