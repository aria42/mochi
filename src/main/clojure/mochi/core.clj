(ns mochi.core
  {:doc "My bag of clojure utility fns."
   :author "aria42" })
      
;;; ------------------------
;;; Map Methods
;;; ------------------------

(defn map-vals
  "returns k -> (f v) for (k,v) in m"
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-keys
  "returns (f k) -> v for (k,v) in m"
  [f m]
  (into {} (for [[k v] m] [(f k) v])))

(defn make-map
  "makes a map by forming x -> (f x) pairs
   if val-fn? and (f x) -> x otherwise"
  ([f xs val-fn?]
    (into {}
      (if val-fn? 
	(map (fn [x] [x (f x)]) xs)
	(map (fn [x] [(f x) x]) xs))))
  ([f xs] (make-map f xs true)))
    
(defn map-invert
  "v->k for (k,v) in m"
  [m]
  (into {} (map reverse m)))

(defmacro map->
  [form aseq]
  `(map #(~(first form) % ~@(rest form)) ~aseq))

(defmacro map->>
  [form aseq]
  `(map #(~(first form) ~@(rest form) %) ~aseq))
	 

;;; ------------------------
;;; Defaults
;;; ------------------------

(defn or-else [x d]
  (if x x d))

(defn update-in-default
  "like update-in but provide a default if value doesn't exist"
  [m ks d f & args]
  (update-in m ks (fn [x] (apply f (if x x d) args))))

(defn update-all-in
  [m ks f & args]
  (update-in-default m ks (list) (fn [x] (map #(apply f % args) x))))

(defn apply-with-default 
  "same as apply but default argument"
  [f dflt & args]
  (fn [x] (apply f (if x x dflt) args)))

(defn default-to [f d] 
  (fn [x] (if x (f x) d)))


;;; ------------------------
;;; Maxes
;;; ------------------------

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

(defn find-max 
  ([f xs]
     (let [[arg-maxes max] (find-maxes f xs)]
       [(first arg-maxes) max]))
  ([xs] (find-max identity xs)))

;;; ------------------------
;;; MISC
;;; ------------------------

(defn safe [f args]
  (try
       (apply f args)
       (catch Exception _ nil)))

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


;;; -------------------------------------
;;; ITransient
;;; Data which can be converted to a 
;;; transient version and backwards
;;; ------------------------------------

(defprotocol ITransient
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


;;; -----------------------
;;; Num Conversions
;;; -----------------------

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
 

;;; ------------------------
;;; IJList
;;; ------------------------

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
      
;;; -------------------------
;;;  def macros
;;; -------------------------      
      
;; (defmacro  defstrict
;;   [name arglist & body]
;;   `(defn ~name
;;      ~(type-tagged-args arglist)
;;      ~(arg-type-preconditions arglist)
;2
;      ~@body))      

(defmacro def-
  "Same as def but yields a private definition"
  [name & decls]
  (list* `def (with-meta name (assoc (meta name) :private true)) decls))


;;; ------------------------
;;; Test
;;; ------------------------

(comment
  (update-all-in {:a [1 2 3]} [:a] inc)
)




