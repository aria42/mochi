(ns mochi.zip-utils
  (:import  [java.io FileOutputStream FileInputStream OutputStreamWriter File]            
            [java.util.zip ZipOutputStream ZipFile ZipEntry GZIPInputStream]))

(defmacro to-zip-entry 
  "binds *out* to an entry in a zip archive. anything
   printed will get sent there"
  [#^ZipOutputStream zipOut #^String entryName & body]
  `(let [zipEntry# (ZipEntry. ~entryName)]
     (.putNextEntry ~zipOut zipEntry#) 
     (binding [*out* (-> ~zipOut OutputStreamWriter.)] 
       ~@body
       (.flush *out*))
     (.closeEntry ~zipOut)))     

;;; Comment 

(comment
(def zout (-> "out.zip" FileOutputStream. ZipOutputStream.))
(macroexpand-1 (to-zip-entry zout "1.txt" (println "Hello")))
(.close zout)  
)
