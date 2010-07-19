(ns mochi.gopts
  (:use [mochi cli core]
    [clojure.contrib def]))

(def- _gargs (atom {}))
(def- _gopts (atom {}))

(defn gargs [] @_gargs)

(defn add-gargs! [args]
  (when (some #{"-h" "-help"} args)
    (do-help (vals @_gopts)))  
  (swap! _gargs into (first (process-args args))))

(defn add-gopts! [& specs]
  (swap! _gopts into  (map (fn [s] [(first s) s]) specs)))  

(defn gopt [name]
  (when-let [[name,req?,doc,f] (@_gopts (keyword name))]
    (let [garg (@_gargs (keyword name))]
      (when (and req? (not garg))
	(throw (RuntimeException. (format "Required opt %s not set" name))))
      (if (instance? Callable f) (f garg) f))))


(comment
  (reset! _gspecs {})
  (reset! _gargs {})
  (add-gopts! [:lambda true "lambda" (default-to to-double 1.0)])
  (add-gargs! ["-lambda" "2"])
  (gopt :lambda)
)