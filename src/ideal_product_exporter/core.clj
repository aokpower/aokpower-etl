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

;; RESUMABLE EXCEPTIONS
;; Doing error handling through a condition system built on top of Clojure's
;; dynamic binding system, as described here:
;; https://www.youtube.com/watch?v=zp0OEDcAro0

(defmacro deferror
  "Defines a dynamic function that takes a msg string and info map, and `throw``s.
  Intended to be used as the foundation of a resumable exception system, instead of
  traditional error handling. Using `binding` and other dynamic vars you can
  define different ways of handling errors without breaking function
  encapsulation or deciding on behalf of other callers how they want to handle
  the error. For an overview of how this works, please see the talk 'Condition
  Systems in an Exceptional Language' by Chris Houser."
  [err-name]
  (list 'defn (vary-meta err-name assoc :dynamic true) ['msg 'info]
        '(throw (ex-info msg info))))

(defmacro deferrorcase
  "Defines a dynamic var with no value. Intended to use in resumable exception system."
  [case-name]
  (list 'def (vary-meta case-name assoc :dynamic true)))

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

(deferror *invalid-product-record-file-error*)
(deferror *invalid-product-file-error*)
(deferrorcase *skip-value*)
(deferrorcase *keep-value*)
(deferrorcase *use-value*)

(defn parse-csv-row
  ([header] ;; partial
   #(parse-csv-row header %))
  ([header row]
   (if (and (= (count header) (count row))) ; add validations to `and` form
     (zipmap header row)
     (binding [*keep-value* identity]
       (*invalid-product-record-error*
        "Row was not well formed, couldn't use."
        {:data row})))))

(defn parse-csv [[header & rows]]
  (let [row-parser (->> (map keywordize header) parse-csv-row)]
    (keep #(binding [*skip-value* (constantly nil)] ; condition case. keep skips nil values.
            row-parser %) rows)))
    ;; (binding [*skip-value* (constantly nil)]
    ;;   (keep row-parser rows))))

(def test-csv-data (with-open [reader (io/reader "/Users/cooperlebrun/RDP_Share/husq_ppl.TXT")]
                     (doall
                      (csv/read-csv reader))))

; Data has invalid rows
(->> test-csv-data (take 3) (map count))

; doesn't raise error
(binding [*invalid-product-record-error* *skip-value*] (parse-csv test-csv-data))
; raises error
(parse-csv test-csv-data)
