(ns ideal-product-exporter.core
  (:require [clojure.data.csv        :as csv]
            [clojure.java.io         :as io]
            [clojure.string          :as string]))
            ;; [clojure.spec.alpha      :as s]
            ;; [clojure.spec.test.alpha :as stest]

;; NOTES
; Should be as lazy as possible to avoid holding all data in memory at once to
; avoid running out of memory when processing large csv files.

;;; UTIL

(defn index [coll]
  "Returns lazy sequence of lists with original coll index as first element."
  (map-indexed (fn [& xs] xs) coll))

(defn words [^String string]
  "Splits STRING on whitespace, returning vector of words."
  (string/split string #"[\W]+"))

(defn keywordize [^String string]
  "Returns a keyword symbol of STRING in pascal case."
  (->> (words string) (map string/capitalize) string/join keyword))

;;; SPEC
;; Empty for now. Clearly I have a lot to learn about how spec works.
;; I can easily create specs for data but the #1 use I had for it was
;; instrumentation and property-based testing for functions, neither
;; of which I can get working for now.

;;; APPLICATION CODE
(defn csv-data->pp-maps
  "Turns csv/read-csv data (vec of vec of strings) into map of :valid and
  :invalid rows. values are lazy seqs so you only compute what you use.
  Optimized for memory: you only allocate/compute what you use."
  [[header-data & rows]]
  (let [header (map keywordize header-data)
        header-length (count header) ; pre-compute
        valid? #(= (count header) (count %))] ; remove rows that have a different number of cells from header
    (map #(zipmap header %) (keep #(when (valid? %) %) rows))))

;; SPIKE
(def spike-csv-filepath "/Users/cooperlebrun/RDP_Share/husq_ppl.TXT")

(def spike-csv-data
   (with-open [reader (io/reader spike-csv-filepath)]
     (doall (csv/read-csv reader))))

(def spike-parsed-data
  (csv-data->pp-maps [["header"] ["data"]]))
