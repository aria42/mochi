(ns mochi.experiment
  (:use [clojure.contrib def singleton duck-streams] 
	[mochi gopts core file-utils])
  (:require [mochi.logger :as logger]))

(declare #^java.io.File +exec-dir+)

(defn- find-exec-dir [#^String pool-dir]
  (ensure-dir (to-file pool-dir))
  (loop [i 0]
    (let [f (java.io.File. pool-dir (str i ".exec"))]
      (if (and (.exists f) (.isDirectory f)) (recur (inc i))
	  f))))

(def- #^java.io.File exec-dir 
  (global-singleton (fn []  
    (if (gopt :exec-dir) (gopt :exec-dir)
      (let [exec-dir (find-exec-dir (gopt :exec-pool-dir))]
	(ensure-dir exec-dir)
	exec-dir)))))

(def- setup 
  (global-singleton (fn []
    (add-gargs! *command-line-args*)
    (add-gopts! [:exec-pool-dir false "pool of dirs" "execs"])
    (logger/add-file! (java.io.File. #^java.io.File (exec-dir) "out.log") false))))
  

(def- setdown 
  (global-singleton (fn []
    (write-lines (java.io.File. +exec-dir+ "gargs.txt")
		 (map (fn [[k v]] (str k " " v)) (gargs)))
  (logger/shutdown))))


(defn run [f]
  (setup)
  (def +exec-dir+ (exec-dir))
  (logger/track (str "Experiment: " +exec-dir+)
    (f))
  (setdown))

(comment
 (defn my-run []
   (logger/<< "hello")
   (logger/to-file (java.io.File. +exec-dir+ "this.txt")
     (logger/<< "should be here")))
		   
 (run my-run)
)

