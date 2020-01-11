(ns fwpd.core)

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn record->name
  [record]
  (:name record))

(defn records->names
  [records]
  (map record->name records))

(defn not-empty?
  [value]
  (not (empty? value)))

(defn not-nil?
  [value]
  (not (nil? value)))

(defn validates
  [validators record]
  (reduce (fn [acc [key validator]]
            (and acc
                 (validator (key record))))
          true
          validators)
  )

(defn append
  [new-record records]
  (if (validates {:name not-empty? :glitter-index not-nil?} new-record)
    (concat records [new-record])
    records)
  )

(defn to-csv-string
  [records]
  (clojure.string/join
    "\n"
    (map (partial clojure.string/join ",")
         (map #(vector (:name %) (str (:glitter-index %)))
              records))))
