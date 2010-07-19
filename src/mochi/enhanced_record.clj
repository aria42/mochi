(ns mochi.enhanced-record
  (:require [clojure.contrib.str-utils2 :as str2])
  (:use [clojure.string :only (join)]
        [clojure.contrib.pprint :only
          (*simple-dispatch* use-method pprint-map pprint)]))

;;;; enhanced records

;; internal helper for maps

(defn remove-values-from-map [value-predicate m]
  (select-keys m (filter #(not (value-predicate (get m %))) (keys m))))

;; internal helpers for name conversion

(defn take-even [x]
  (take-nth 2 x))

(defn take-odd [x]
  (take-nth 2 (drop 1 x)))

(defn camel-to-dashed
  "Convert a name like 'BigBlueCar' to 'big-blue-car'."
  [s]
  (let [parts (drop 1 (str2/partition s #"[A-Z]"))]
    (join "-" (map #(str %1 %2)
                   (map str2/lower-case (take-even parts))
                   (take-odd parts)))))

;; internal helpers for changing records via maps

(defn set-record-field
  "Set a single field on a record."
  [source [key value]]
  (assoc source key value))

(defn set-record-fields
  "Set many fields on a record, from a map."
  [initial value-map]
  (reduce set-record-field initial value-map))

;; NOTE: The way that fields are set in this approach is inefficient and garbage
;; objects are thrown off when setting multiple fields, but it does present the
;; abstraction we want of setting fields via maps.

;; internal helper for generating constructor function

(defmacro make-record-constructor
  "Define the constructor functions used to instantiate a record."
  [ctor-name default-record]
  `(defn ~ctor-name
     ([value-map#]
        (~ctor-name ~default-record value-map#))
     ([initial# value-map#]
        (set-record-fields initial# value-map#))))

;; internal helpers for printing

(defmacro print-record
  "Low-level function to print a record to a stream using the specified constructor
   name in the print output and using the provided write-contents function to write
   out the contents of the record (represented as a map)."
  [ctor-name record stream write-contents]
  `(do
     (.write ~stream (str "(" ~ctor-name " "))
     (~write-contents (remove-values-from-map nil? ~record))
     (.write ~stream  ")")))

(defn print-record-contents
  "Simply write the contents of a record to a stream as a string. Used for basic
   printing."
  [stream contents]
  (.write stream (str contents)))

(defmacro setup-print-record-method [ctor-name type-name method-name]
  `(defmethod ~method-name ~type-name [record# writer#]
    (print-record ~ctor-name record# writer# (partial print-record-contents writer#))))

(defmacro setup-print-record
  "Define the print methods to print a record nicely (so that records will print in
   a form that can be evaluated as itself)."
  [ctor-name type-name]
  `(do
     (setup-print-record-method ~ctor-name ~type-name print-method)
     (setup-print-record-method ~ctor-name ~type-name print-dup)))

(defn generate-record-pprint
  "Return a function that can be used in the pprint dispatch mechanism to handle a
   specific constructor name."
  [ctor-name]
  (fn [record]
    (print-record ctor-name record *out* pprint-map)))

;; public entry point

(defmacro def-enhanced-record
  "Defines a record and sets up constructor functions, printing, and pprinting for
   the new record type."
  ([type-name field-list]
     `(defrecord2 ~type-name ~field-list
        ;; invoke defrecord2 with default constructor function name
        ~(symbol (str "new-" (camel-to-dashed (str type-name))))))
  ([type-name field-list ctor-name]
      `(do
         ;; define the record
         (defrecord ~type-name ~field-list)
         ;; define the constructor functions
         (make-record-constructor ~ctor-name
                                  (~(symbol (str type-name "."))
                                     ~@(repeat (count field-list) nil)))
         ;; setup printing
         (setup-print-record (quote ~ctor-name) ~type-name)
         ;; setup pprinting
         (use-method *simple-dispatch* ~type-name
               (generate-record-pprint (quote ~ctor-name))))))
