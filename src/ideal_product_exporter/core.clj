(ns ideal-product-exporter.core
  (:require [clojure.data.csv   :as csv]
            [clojure.java.io    :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;;; UTIL

(defn index [coll]
  "Returns lazy sequence of lists with original coll index as first element."
  (map-indexed (fn [& xs] xs) coll))

(defn words [^String string]
  "Splits STRING on whitespace, returning vector of words."
  (str/split string #"[\W]+"))

(defn keywordize [^String string]
  "Returns a keyword symbol of STRING in pascal case."
  (->> (words string) (map str/capitalize) str/join keyword))

;;; TEMP, DEBUG
(def test-fpath "/Users/cooperlebrun/RDP_Share/husq_ppl.TXT")

;;; SPEC
(s/def ::same-n-of-cells #(apply = (map count %)))
(s/def ::same-brands #(apply = (map first (rest %))))
(s/def ::valid-net-prices
  (fn [candidate]
    (let [net-price-re #"[\d,]+\.\d+"]
      (->> candidate
           (rest)
           (map last)
           (every? #(re-matches net-price-re %))))))
(s/def :ideal/inventory-export (s/and ::same-brands
                                      ::valid-net-prices
                                      ::same-n-of-cells
                                      (s/coll-of vector? :distinct true)))

;;; CODE

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; first row is header
            keywordize
            repeat)
       (rest csv-data)))


(defn import-product-price-list
  "Reads FILEPATH into a csv, validates, removes duplicates and normalizes, then
  transforms into lazy collection of maps based on csv header."
  [filepath]
  (let [csv (try (with-open [reader (io/reader filepath)]
                   (doall
                    (csv/read-csv reader)))
                 (catch))
        has-correct-row-lengths (s/valid? ::same-n-of-cells)]))
