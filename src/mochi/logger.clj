(ns mochi.logger  
  (:require [mochi core file-utils [counter :as cntr]]))

;;; ------------------------
;;; Globals
;;; ------------------------

(def +indent-level+ (atom (cntr/make)))
(def +start-times+ (atom (list)))
(def +log-outs+  (atom (list *out*)))

;;;------------------
;;; Methods
;;;------------------

(defn- indent [out] 
  (apply str (take (@+indent-level+ out) (repeatedly (fn [] "  ")))))

(defn <<
  "Major output for logger"
  [& msgs] 
  (doseq [#^java.io.Writer o @+log-outs+]
    (.write o #^String (apply str (indent o) msgs))
    (.write o "\n")))

(defn start-track
  [track-name]
  (<< track-name " {")
  (swap! +start-times+ conj (System/currentTimeMillis))
  (doseq [o @+log-outs+] (swap! +indent-level+ cntr/inc-count o 1)))

(defn- time-str [secs]
  (cond
    (> secs (* 60 60)) 
      (str (/ secs (* 60 60)) "h" (time-str (mod secs (* 60 60))))
    (> secs 60.0) 
      (str (/ secs 60) "m" (time-str (mod secs 60)))      
    :else 
      (format "%.3fs" secs)))

(defn end-track []
  (doseq [o @+log-outs+] (swap! +indent-level+ cntr/inc-count o -1)) 
  (when (empty? (deref +start-times+))
    (throw (RuntimeException. "logger/end-track called without start-track")))
  (let [start-time (first (deref +start-times+))
	stop-time  (System/currentTimeMillis)
	secs  (/ (- stop-time start-time) 1000.0)]
    (<< "}" (format "[%s]" (time-str secs)))
    (swap! +start-times+ pop)     
    nil)) 
    	   		
(defmacro track
  [track-name & body]
  `(do (start-track ~track-name)
       (let [res# (do ~@body)]
          (end-track)
          res#)))

(defmacro to-file
  [f & body]
  `(let [fout# (java.io.FileWriter. ~f)]
     (swap! +log-outs+ conj fout#)
     (let [res# (do ~@body)]
       (.close fout#)
       (swap! +log-outs+ rest)
       res#)))

(defn add-file!
  [f #^Boolean append?]
  (swap! +log-outs+ conj (java.io.FileWriter. #^java.io.File (mochi.file-utils/to-file f) append?)))
  
(defmacro append-to-file
  [f & body]
  `(let [fout# (java.io.FileWriter. (mochi.core/to-file ~f) true)]
     (swap! +log-outs+ conj fout#)
     (let [res# (do ~@body)]
       (.close fout#)
       (swap! +log-outs+ rest)
       res#)))

(defn shutdown []
  (doseq [#^java.io.Writer o @+log-outs+] 
    (try (.flush o)
      (catch Exception e))))
