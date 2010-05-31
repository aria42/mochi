;; (ns mochi.ml.seq-model
;;   (:import [edu.berkeley.nlp.math SloppyMath]
;; 	   [incanter Matrix]
;; 	   [cern.colt.matrix.tdouble.impl DenseDoubleMatrix1D])
;;   (:use [incanter core] [clojure.contrib [seq-utils :exclude [group-by]]] [mochi sloppy-math]))

;; (deftype State [label index])  

;; (deftype StateSpace
;;     [states          ; vec of states, each state [label index]
;;      start-state     ; start state
;;      stop-state      ; stop state
;;      index-fn        ; map :label => [label index]
;;      trans           ; vec of [start stop] state pairs
;;      trans-by-start  
;;      trans-by-stop])

;; (declare make-states make-trans make-label2state)

;; (def old-StateSpace StateSpace)

;; (defn make-state-space 
;;   "Make a state-space-struct:
;;    labels:  seq of labels, should include start-state, stop-state
;;    start-label: should be in labels
;;    stop-label:  should be in labels
;;    trans-labels: seq of ordered pair of allowed labels. Should
;;    include outgoing transitions for start-label and incoming
;;    for stop-label"
;;   ([labels start-label stop-label trans-labels]
;;      (let [states (make-states labels) by-label (make-label2state states)
;; 	   trans (make-trans trans-labels by-label)]
;;        (old-StateSpace
;;          states (by-label start-label) (by-label stop-label) by-label trans
;; 	 (vec (for [s states] (filter (fn [[t _ _]] (= s t)) trans)))	     
;; 	 (vec (for [s states] (filter (fn [[_ t _]] (= s t)) trans))))))	     
;;   ([labels start-label stop-label]
;;      (make-state-space labels start-label stop-label
;;        (concat (for [s labels] [start-label s])	       
;; 	       (for [s labels t labels] [s t])
;; 	       (for [s labels] [s stop-label])))))

;; (defn make-states [labels]
;;   (map (fn [[index label]] [label index]) (indexed labels)))

;; (defn make-label2state  [states]
;;   (persistent! 
;;    (reduce (fn [res state] (assoc! res (:label state) state
;;   (loop [states states res (transient {})]
;;    (if-let [[label index] (first states)]
;;      (recur (rest states) (assoc! res label [label index]))
;;      (persistent! res))))
       	 
;; (defn make-trans [trans-labels label2state]
;;   (vec (map (fn [[index [s t]]] [s t index])
;;          (indexed (for [[s t] trans-labels] [(get label2state s) (get label2state t)])))))

;; ;; Forwards Backward

;; (defn compute-alphas [state-space #^Matrix potentials]
;;   (let [#^int input-length (-> potentials nrow dec int)
;; 	#^int num-states (-> state-space :states count int)
;; 	#^int num-trans (-> state-space :trans count)
;; 	#^Matrix alphas (incanter.core/matrix Double/NEGATIVE_INFINITY input-length num-states)]
;;     (println "num-states " num-states)
;;     ;; Starting
;;     (let [[_ start-index] (-> state-space :start-state)]
;;       (.setQuick alphas 0 start-index 0.0))
;;     ;; Induction
;;     (dotimes [i input-length]
;;       (when (> i 0)
;; 	(dotimes [s num-states]
;; 	  (let [outbound (-> state-space :trans-by-stop (nth s))
;; 		trans-scores (DenseDoubleMatrix1D. (int (count outbound)))
;; 		last-i (-> i dec int)]
;; 	    (doseq [pair (indexed outbound)
;; 		:let [index (first pair)
;; 		      transition (second pair)
;; 		      from-state-index (-> transition first second int) 			 
;; 		      trans (-> transition last int)]]
;; 		  (.setQuick trans-scores (int index)
;; 		    (+ (double (.getQuick alphas last-i from-state-index))
;; 		       (double (.getQuick potentials last-i trans)))))	   
;; 	    (.se#^int tQuick alphas (int i) (int s) (SloppyMath/logAdd #^doubles (.elements trans-scores)))))))
;;     alphas))

;; ;; (defn compute-alphas-2 [state-space #^Matrix potentials]
;; ;;   (let [input-length (-> potentials nrow dec int)
;; ;; 	num-states (-> state-space :states count int)
;; ;; 	num-trans (-> state-space :trans count)
;; ;; 	#^Matrix alphas (incanter.core/matrix Double/NEGATIVE_INFINITY input-length num-states)]
;; ;;     ;; Starting
;; ;;     (let [[_ start-index] (-> state-space :start-state)]
;; ;;       (.setQuick alphas 0 start-index 0.0))
;; ;;     ;; Induction
;; ;;     (dotimes [i input-length]
;; ;;       (when (> i 0)
;; ;; 	(dotimes [s num-states]
;; ;; 	  (let [outbound (-> state-space :trans-by-stop (nth s))
;; ;; 		trans-scores  
;; ;; 		(for [transition outbound] ;[[_ from-state-index] _ trans]
;; ;; 		  (let [last-i (-> i dec int)
;; ;; 			from-state-index (-> transition first second int) 
;; ;; 			trans (-> transition last int)]
;; ;; 		      (+ (double (.getQuick alphas last-i from-state-index))
;; ;; 		         (double (.getQuick potentials last-i trans)))))]
;; ;; 	    (.setQuick alphas (int i) (int s) (log-add trans-scores))))))
;; ;;     alphas))


;; ;; (defn forwards-backward 
;; ;;   ""
;; ;;   [state-space potentials]
;; ;;   (let [alphas (compute-alphas state-space potentials)
;; ;; 	betas  (make-array Double/TYPE n m)
;; ;; 	node-posts (make-array Double/TYPE n m)
;; ;;         edge-posts (make-array Double/TYPE (dec n) num-trans)]
;; ;;     ;; Alphas 
;; ;;     (dotimes [i input-length))
	
	

