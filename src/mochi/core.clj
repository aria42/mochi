(ns mochi.core
  {:doc "My bag of clojure utility fns."
   :author "aria42" }
  (:require [clojure.contrib [io :as io]]))
      
;;; Map Methods ;;;

(defn map-vals
  "returns k -> (f v) for (k,v) in m"
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn sum
  "returns sum (f x) for x in xs"
  ([f xs]
     (reduce
      (fn [res x] (+ res (f x)))
      0.0 xs))
  ([xs] (reduce + xs)))

(defn map-keys
  "returns (f k) -> v for (k,v) in m"
  [f m]
  (into {} (for [[k v] m] [(f k) v])))

(defn deep-merge-with
  "Recursely merge-with for nested maps"
  [f & maps]
  (apply
   merge-with
   (fn [x y]
     (if (and (map? x) (map? y)) (deep-merge-with f x y)
	 (f x y)))
   maps))

(defn make-map
  "makes a map by forming x -> (f x) pairs
   if val-fn? and (f x) -> x otherwise"
  ([f xs val-fn?]
    (into {}
      (if val-fn? 
	(map (fn [x] [x (f x)]) xs)
	(map (fn [x] [(f x) x]) xs))))
  ([f xs] (make-map f xs true)))
    
(defmacro map->
  "performs map with argument inserted after fn in form"
  [form aseq]
  `(map #(~(first form) % ~@(rest form)) ~aseq))

(defmacro map->>
  "performs map with argument insterted at end of form"
  [form aseq]
  `(map #(~(first form) ~@(rest form) %) ~aseq))
	 
;;; Handling Defaults ;;;

(defn or-else [x d]
  "returns d if not x"
  (if x x d))

(defn update-in-default
  "like update-in but provide a default if value doesn't exist"
  [m ks d f & args]
  (update-in m ks (fn [x] (apply f (if x x d) args))))

(defn update-all-in
  "Like update-in but expects a seq of items, maps (f elem args)
  over each element."
  [m ks f & args]
  (update-in-default m ks (list) (fn [x] (map #(apply f % args) x))))

(defn default-to
  "returns a wrapper around f that returns d if arg to f is nil"   
  [f d]
  (fn [x] (if x (f x) d)))

(defn default-result-to
  "returns a wrapper around f that returns d if arg to f is nil"   
  [f d]
  (fn [& args] (if-let [res (apply f args)] res d)))

;;; Maxes ;;;

(defn find-maxes 
  "returns [arg-maxes max] tuple of f over xs"   
  ([f xs]
     (loop [arg-maxes [] max nil xs xs]
       (if-let [x (first xs)]
	 (let [fx (f x)]
	   (cond
	    (or (nil? max) (> fx max)) (recur [x] fx (rest xs))
	    (= fx max) (recur (conj arg-maxes x) max (rest xs))
	    :default (recur arg-maxes max (rest xs))))
	 [arg-maxes max])))
  ([xs] (find-maxes identity xs)))

;;; Control ;;;

(defn safe
  "Wraps f execution in a try catch and
   returns nil on exception."
  [f]
  (fn [& args]
    (try
       (apply f args)
       (catch Exception _ nil))))

(defmacro die-on-error [form & msgs]
  `(try
      ~form
      (catch Exception e#
	(.println System/err (str ~@msgs))
	(.printStackTrace e#)
	(System/exit 0))))
      
(defmacro try-or-else [form deflt]
  `(try
      ~form
      (catch Exception e#
	~deflt)))

;;; Function Manipulation ;;;

(defn rpartial
  "right curry args, might be inefficient"
  ([f & args]
     (fn [& prefix-args]
       (apply f (concat prefix-args args)))))

(defn rcomp
  "composes function f1 f2 f3 in reverse order
   so (f3 (f2 (f1 x))), when you process data
   you can think of the functions as happening
   in order"
  [& fs]
  (apply comp (reverse fs)))

;;; ITransient ;;;
;;; Data which can be converted to a 
;;; transient version and backwards

(defprotocol ITransient
  "Make a transient/persistent version of an object"
  (transient? [_] "is transient")
  (to-transient [_] "makes a transient version")
  (to-persistent! [_] "makes a persistent version"))

(extend-protocol ITransient

  clojure.lang.IEditableCollection
  (transient? [x] false)
  (to-transient  [x] (transient x))

  clojure.lang.ITransientCollection
  (transient? [x] true)
  (to-persistent! [x] (persistent! x)))

;;; Num Conversions ;;;

(defprotocol INumConvert
  (#^int to-int [x] "make an int")
  (#^double to-double [x] "make a double"))

(extend-protocol INumConvert
  
  String
  (to-int [x] (Integer/parseInt x))
  (to-double [x] (Double/parseDouble x))
  nil
  (to-int [x] (int x))
  (to-double [x] (double x)))
 
;;; IJList ;;;

(defprotocol IJList
  (#^java.util.List to-jlist [x] "converts input to java.util.List"))

(extend-protocol IJList
  
  java.util.List
  (to-jlist [x] x)

  clojure.lang.Seqable
  (to-jlist [x] 
    (let [res (java.util.ArrayList.)]
      (doseq [elem (seq x)] (.add res elem))
      res))) 
      
;;;  def macros ;;;
      
(defmacro def-
  "Same as def but yields a private definition"
  [name & decls]
  (list* `def (with-meta name (assoc (meta name) :private true)) decls))

(defn- decorate-hinted-symbol 
   [sym] 
   (let [[arg type] (.split (name sym) ":")] 
     (if type 
       (with-meta (symbol arg) {:tag (symbol type)}) 
       sym))) 

(defn- decorate-hinted-args
   [args] 
   (cond 
     (vector? args) (vec (map decorate-hinted-args args)) 
     (map? args) (into {} (map decorate-hinted-args args)) 
     (symbol? args) (decorate-hinted-symbol args) 
     (keyword? args) args 
     :else (throw (Exception. (str args))))) 

(defmacro defh 
   [name args & body] 
   `(defn ~name 
      ~(decorate-hinted-args args) 
      ~@body)) 

;;; IO-ish ;;;

(defn ls [f]
  (seq (.listFiles (io/file f))))


;;; Indexer ;;;

(defn indexer [xs]
  (let [xs (if (set? xs) xs (into #{} xs))
	index-fn (into {} (map-indexed (fn [i x] [x i]) xs))]
    [xs (fn [x] (get index-fn x -1))]))

