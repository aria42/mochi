
(ns mochi.search
  {:doc "Generic AI Search Routines; by virtue of generality, this code
   isn't as efficient for any particular problem, but is useful
   for problems which don't require strong performance."
   :author "Aria Haghighi (aria42@gmail.com"}

  (:use [mochi core])
  (:import [java.util PriorityQueue]))

;;; Basic Search Problem Abstraction ;;;

(defprotocol ISearchProblem
  (is-goal? [this state] "state goal test")
  (init-state [this] "initial state of problem")
  (successors [this state] "seq of [neighbor step-cost]"))

;;; Search Algorithm and Data Structures ;;;

(defrecord #^:private SearchNode
  [path cost-so-far priority]
  java.lang.Object
  (toString [this] (format "%s %s" path cost-so-far priority)))

(defn- pq-new []
  (PriorityQueue. 20
     (comparator
      (fn [#^SearchNode x #^SearchNode y] (< (.priority x) (.priority y))))))

(defn- init-search-node [search-problem]
  (SearchNode. (list (init-state search-problem)) 0.0 0.0))

(defn- generic-search
  ""
  [#^ISearchProblem search-problem priority-fn]
  (let [pq (doto (pq-new) (.add (init-search-node search-problem)))]	
    #_(swank.core/break)
    (loop [ignore nil]
      (let [#^SearchNode node (.poll pq) path (.path node) state (first path)]
	#_(swank.core/break)
	(if (is-goal? search-problem state) (reverse path) 
	  (recur
	   (doseq [[succ cost] (successors search-problem state)
		   :let [g (+ (.cost-so-far node) cost)]]
	     (.add pq (SearchNode. (cons succ path)
				   g
				   (priority-fn node succ cost))))))))))

(defn astar-search
  [search-problem heuristic-fn]
  (generic-search search-problem
		  (fn [#^SearchNode node succ cost]
		    (+ (.cost-so-far node) cost (heuristic-fn succ)))))

(defn uniform-cost-search
  [search-problem]
  (astar-search search-problem (constantly 0.0)))

;;; Graph Search as Search Problem ;;;

(defrecord GraphPathSearchProblem [graph start goal]
  ISearchProblem
  (is-goal? [this state] (= state goal))
  (init-state [this] start)
  (successors [this state] (get graph state [])))

(comment

  (uniform-cost-search
   (GraphPathSearchProblem.
    {:s [[:a 1.0],[:b 0.5]] :a [[:g 1.0]] :b [[:g 1.0]]} :s :g))
  
)

