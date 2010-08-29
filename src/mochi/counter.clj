(ns mochi.counter
  {:doc "Counter abstraction. Essentially a map between keys
         and counts. Don't just use a map since you may want to
         cache things like the total count. Forms the basis
         for a lot of discrete probability code. "
   :author "Aria Haghighi"}
  (:use [mochi core])
  (:require [mochi.sloppy-math :as sloppy-math]))

;;; ICounter Protocol ;;;

(defprotocol ICounter
  (total-count [c] "total-count of all key value pairs")
  (get-count [c k] "get count")
  (inc-count [c k v] "inc count, return a new counter with updated counts")
  (all-counts [c] "a map from keys to counts"))

(defn set-count [counter k v]
  (inc-count counter k (- v (get-count counter k))))  

(extend-protocol ICounter
  ; So a normal map is treated like a counter
  clojure.lang.IPersistentMap
  (total-count [this] (sum (vals this)))
  (get-count [this k] (get this k 0.0))
  (inc-count [this k v] (assoc this k  (+ (get this k 0.0) v)))
  (all-counts [this] this)
  ; As is a transient map
  clojure.lang.ITransientMap
  (total-count [this] (sum (vals this)))
  (get-count [this k] (get this k 0.0))
  (inc-count [this k v] (assoc! this k  (+ (get this k 0.0) v)))
  (all-counts [this] this))


; The counter type exists, to
; cache the total as well as
; add things like IFn support
; and a unique toString
(deftype Counter [counts total]

  clojure.lang.Seqable
  (seq [this] (seq (.counts this)))

  clojure.lang.Counted
  (count [this] (count (.counts this)))

  clojure.lang.IFn
  (invoke [this k] (get (.counts this) k 0.0))
  
  java.lang.Object
  (toString [this] (str (.counts this)))
  (hashCode [this] (.hashCode (.counts this)))

  clojure.lang.IPersistentCollection

  clojure.lang.IEditableCollection
  (asTransient [this] (Counter. (transient counts) total))

  clojure.lang.ITransientCollection
  (persistent [this] (Counter. (persistent! counts) total))
  
  IsTransient
  (transient? [this] (transient? (.counts this)))

  ICounter
  (total-count [this] (.total this))
  (get-count [this k] (get (.counts this) k 0.0))
  (all-counts [this] (.counts this))
  (inc-count [this k v]
    (let [assoc-fn (if (transient? this) assoc! assoc)]
      (Counter.
       (assoc-fn (.counts this) k (+ v (get-count this k)))
       (+ (.total this) v)))))

(defn make
  "ICounter Factory"
  ([] (Counter. {} 0.0))
  ([counts] (Counter. counts (reduce + (map second counts)))))

;;; Methods That Make a new ICounter ;;;

(defn map-counter 
  [f counter]
  (make (map-vals f (all-counts counter))))

(defn scale
  [counter alpha]
  (map-counter #(* alpha %) counter))

(defn normalize
  [counter]
  (scale counter (/ 1 (total-count counter))))

(defn merge-counters
  "adds counts from all input counters"
  [& counters]
  (Counter.
   (apply merge-with + (map all-counts counters))
   (sum  (map total-count counters))))

;;; Mathy Methods ;;;

(defn log-normalize
  "for a counter, where counts represent lg(x) counts
   returns log-sum = lg(um_i x_i) as well as
   counts which subtract log-sum from each value"
  [counter]
  (let [log-sum (sloppy-math/log-add (map second counter))]
    [log-sum (map-counter #(- % log-sum) counter)]))

(defn log-scores-to-probs [counter]
  (let [[log-sum log-counts] (log-normalize counter)]
    [log-sum (map-counter #(Math/exp %) log-counts)]))

(defn find-max [counter]
  (apply max-key second (all-counts counter)))

(comment
  (def c (make))  
  (class (.counts c))
  (transient? c)
  (transient? (transient c))
  (transient? c)
  ;
	(time
   (persistent!
	 (reduce
	  (fn [counts key] (inc-count counts key 1.0))
	  (transient (make))
	  (take 1e5 (repeatedly (constantly :a))))))
  ;
	(time
   (reduce
    (fn [counts key] (inc-count counts key 1.0))
    (make)
    (take 1e5 (repeatedly (constantly :a)))))
  ;
  (def c (-> (make) (inc-count :a 1.0) (inc-count :b 2.0)))
  (all-counts c)
  (find-max c)
  (merge-counters c c)
  (log-normalize (inc-count c :a -1.0)) 
  (log-scores-to-probs (inc-count c :a -1.0))
  x
  (inc "a")
  (-> (make)
      (inc-count :a 1.0)
      (inc-count :b 2.0)
      (scale 2.0)
      normalize)
  )

