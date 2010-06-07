(ns mochi.control)

;;; ------------------------
;;; Control Structure
;;; ------------------------

(defn die-when
  [pred x]
  (if (pred x) 
    (throw (RuntimeException. (str "Die: Bad " x)))
    x))

(defn die-unless  
  [pred x]
  (if (not (pred x))
    (throw (RuntimeException. (str "Die: Bad " x)))
    x))


