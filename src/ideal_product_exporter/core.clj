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

;;; CODE

(deferror *invalid-product-record-file-error*)
(deferror *invalid-product-file-error*)
(deferrorcase *skip-value*)
(deferrorcase *keep-value*)
(deferrorcase *use-value*)

(def string-or-number? #(or (juxt number? string?)))

(s/def ::dollar-w-comma-str (s/and string? #(re-matches #"[\d,]+\.\d+" %)))
(s/def ::csv-data-row (s/coll-of string?))
(s/def ::csv-data (s/cat :header (s/? ::csv-data-row)
                         :rows (s/* ::csv-data-row)))
(s/def ::pp-header (s/coll-of keyword? :distinct true))
(s/def ::pp-map (s/map-of keyword? string-or-number?))

(s/fdef csv-row->pp-map
  :args (s/alt :unary (s/cat :header ::pp-header)
               :binary (s/cat :header ::pp-header :row ::csv-data-row))
  :ret (s/alt :curried fn?
              :applied ::pp-map)
  :fn (s/and #(condp get-in %
                [:ret :applied] ; function has been applied
                (let [arg-rows (get-in % [:args :binary :rows])
                      ret-rows (get-in % [:ret :applied])]
                  (<= (count ret-rows) (count arg-rows))) ; return should be equal or shorter
                true))) ; otherwise true
(defn csv-row->pp-map
  ([header] ;; partial
   #(csv-row->pp-map header %))
  ([header row]
   (if (and (= (count header) (count row))) ; add validations to `and` form
     (zipmap header row)
     (binding [*keep-value* identity]
       (*invalid-product-record-error*
        "Row was not well formed, couldn't use."
        {:data row})))))

(s/fdef csv->pp-maps
  :args ::csv-data
  :ret (s/coll-of ::pp-map :distinct true)
  :fn (fn [{args :args ret :ret}]
        (<= (count ret) (count (:rows args)))))

(defn csv->pp-maps [[header & rows]]
  (let [row-parser (->> (map keywordize header) csv-row->pp-map)]
    (keep #(binding [*skip-value* (constantly nil)] ; condition case. keep skips nil values.
            row-parser %) rows)))


(def test-csv-data (with-open [reader (io/reader "/Users/cooperlebrun/RDP_Share/husq_ppl.TXT")]
                     (doall
                      (csv/read-csv reader))))


;; doesn't raise error
;(binding [*invalid-product-record-error* *skip-value*] (csv->pp-maps test-csv-data))
;; raises error
;(parse-csv test-csv-data)
