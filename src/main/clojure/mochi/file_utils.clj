(ns mochi.file-utils 
  (:require [clojure.contrib.duck-streams :as ds])
  (:import [java.io File]))

;;; ------------------------
;;; to-file
;;; ------------------------

(defprotocol IFileable
  (#^java.io.File to-file [f] "make a file view")
  (ensure-dir [f] "ensure directory exists"))

(extend-protocol IFileable

  java.io.File
  (#^java.io.File to-file [f] f)
  (ensure-dir [f]
    (cond
       (.isDirectory f) nil
       (.exists f) (throw (RuntimeException. (str "called ensure-dir on file " f)))
       :else (.mkdirs f)))

  String
  (to-file [s] (java.io.File. s))
  (ensure-dir [s] (ensure-dir (to-file s))))

(defprotocol IPathable
  (#^String to-path [f] "path string"))

(extend-protocol IPathable
  java.io.File 
  (to-path [f] (.getPath f))

  String
  (to-path [s] s))

(defn paths-to-file 
  [& paths]
  (println paths)
  (reduce  (fn [#^java.io.File f p] (java.io.File. f #^String (to-path p)))
	   (to-file (first paths))
	   (rest paths)))


;;; --------------
;;; ext-split
;;; --------------

(defmulti ext-split type)

(defmethod ext-split String [#^String s]
  (let [index (.lastIndexOf s ".")]
    (if (>= index 0)
      [(.substring s 0 index) (.substring s (inc index))]
      (throw (RuntimeException. (str "No ext in " s))))))

(defmethod ext-split File [#^File f]
  (-> f .getName ext-split))

;;; -------------
;;; file-ext
;;; -------------

(defn file-ext [s] (second (ext-split s)))
(defn file-base [s] (first (ext-split s)))

;;; ----------------
;;; change-ext
;;; ----------------

(defmulti change-ext type)

(defmethod change-ext String [s #^String new-ext]
  (let [[base ext] (ext-split s)]
    (str base
         (if (.startsWith new-ext ".")
           new-ext
           (str "." new-ext)))))

(defmethod change-ext File [#^File file new-ext]
  (-> file .getPath #^String (change-ext new-ext) File.))

(defn change-dir 
  { :doc "returns file with same name as f in new-parent directory."      
    :tag File }
  [f new-parent]   
  (let [f (to-file f)]
    (java.io.File (to-file new-parent) (.getName f))))


(defn resource-to-temp-file   
  { :doc "write input-stream to temp file, return file"
    :tag File }
  [resource-name file-ext]
  (let [temp-file (doto (File/createTempFile "mochi" file-ext) .deleteOnExit)]
    (ds/copy 
        (ClassLoader/getSystemResourceAsStream resource-name) 
	temp-file)
    temp-file))

(comment
  (paths-to-file (java.io.File. "/Users/aria42" "out.txt"))
)
