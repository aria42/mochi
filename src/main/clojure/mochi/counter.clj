(ns mochi.counter
  (:require [mochi.sloppy-math :as sloppy-math])
  (:use [mochi core]))
  
;;; ------------------------
;;; ICounter Protocol
;;; ------------------------

(defprotocol ICounter
  (total-count [c] "total-count")
  (get-count [c k] "get count")
  (inc-count [c k v] "inc count"))
  
(defn set-count [counter k v]
  (inc-count counter k (- v (get-count counter k))))  
  
    
(deftype Counter [counts total]

  ICounter
  (total-count [this] total)
  (get-count [this k] (get counts k 0.0))
  (inc-count [this k v] 
    (Counter. 
       ((if (transient?  this)        
         assoc! assoc) 
	       counts  k (+ v (get counts k 0.0))) 				
     	 (+ total v)))

  clojure.lang.Seqable
  (seq [this] (seq counts))

  clojure.lang.Counted
  (count [this] (count counts))

  clojure.lang.IFn
  (invoke [this k] (get counts k 0.0))

  ITransient
  (transient? [this] (transient? counts))
  (to-transient [this] (Counter. (transient counts) total))
  (to-persistent! [this] (Counter. (persistent! counts) total))
    
  java.lang.Object
  (toString [this] (str counts))
  (hashCode [this] (.hashCode counts)))
  

(defn make
  "ICounter Factory"
  ([] (Counter. {} 0.0))
  ([counts] (Counter. counts (reduce + (map second counts))))
  ([counts total] (Counter. counts total)))
	 
;;; ------------------------
;;; make
;;; ------------------------

;; (defmulti make-counter type)

;; (defmethod make-counter clojure.lang.Seqable
;;   [xs]
;;   (loop [xs xs counts (transient {}) total 0]
;;     (if-let [x (first xs)]
;;       (recur (rest xs)
;; 	     (assoc! counts
;; 		     x
;; 		     (inc (get counts x 0.0)))
;; 	     (inc total))
;;       (ICounter. (persistent! counts)  total))))

    
;;; ---------------------------------
;;; Methods That Make a new ICounter
;;; ---------------------------------

(defn map-counter 
  "counter: supports [k v] seq view"
  [f counter]
  (make (map-vals f counter)))

(defn scale
  [counter alpha]
  (map-counter #(* alpha %) counter))

(defn normalize
  [counter]
  (scale counter (/ 1 (total-count counter))))


(defn merge-counters [& counters]
  (Counter.
    (apply merge-with + (map :counts counters))
    (apply + (map total-count counters))))


;;; ---------------
;;; Mathy Methods
;;; ---------------

(defn log-normalize [counter]
  (let [log-sum (sloppy-math/log-add (map second counter))]
    [log-sum (map-counter #(- % log-sum) counter)]))


(defn log-scores-to-probs [counter]
  (let [[log-sum log-counts] (log-normalize counter)]
    [log-sum (map-counter #(Math/exp %) log-counts)]))

(comment
  (def c (make))
  (log-normalize (inc-count c :a -1.0)) 
  (log-scores-to-probs (inc-count c :a -1.0))
  (-> (make)
      (inc-count :a 1.0)
      (inc-count :b 2.0)
      (scale 2.0)
      normalize)
)