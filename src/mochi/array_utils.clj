(ns mochi.array-utils
  { :author "aria42"
   :doc "Lifted largely from somewhere..."})

(defn array? 
  "is this an array"
  [x] (-> x class .isArray))

(defn see 
  [x] (if (array? x) (map see x) x))

(defmacro deep-aget
  ([hint array idx]
    `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
    `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
       (deep-aget ~hint a# ~@idxs))))

(defmacro deep-aset! [hint array & idxsv]
  (let [hints '{doubles double ints int floats float shorts short}
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(deep-aget ~'objects ~array ~@idxs)
                        array)
        a-sym (with-meta (gensym "a") {:tag hint})]
      `(let [~a-sym ~nested-array]
         (aset ~a-sym ~idx ~v))))

(defn double-ainc! [#^doubles array i inc-amt]
  (let [v (aget array i)]
    (aset-double  array i  (+ v inc-amt))
    array))

(comment
  (let [a (double-array 10)]
    (seq (double-ainc! a 1 2.0))))