;;; A quick script to convert JAPN Vocab Lists into CSV files for anki to make decks from
;;; Currently a bug where lines without a definition-starter are getting attached to the previous line

(ns jpnvocab-to-anki.core
  (:require [clojure.java.io :as io]
            [pdfboxing.text :as text]
            [clojure.string :refer [split trim join]])
  (:gen-class))



(def definition-starters #{\1 \2 \3 \＊ \カ \V \L \space})

(def hiragana-katakana-regexp #"[\u3040-\u309f\u30A0-\u30FF\u31F0-\u32FF\uFF00-\uFFEF]+")


(defn remove-extra [defn-list]
  (filter #(not= \V (first %)) defn-list))

(defn merge-definitions [text-list]
  (drop 1
        (second
         (reduce (fn [[definition accum] current]
                   (if (definition-starters (first current))
                     [(trim current) (conj accum definition)]
                     [(str definition " "(trim current)) accum]))
                 ["" []] text-list))))

(defn drop-numbering [defn-string]
  (trim (.substring defn-string 2)))

(defn create-csv-pairs
  [defn-string]
  (if-not (= (first defn-string) \い)
    (let [[grammar & definition] (map trim (split defn-string hiragana-katakana-regexp 2))
          japanese (re-find hiragana-katakana-regexp defn-string)]
      (str japanese "\t" grammar "\t" (apply str definition)))
    (let [[grammar japanese & definition] (split defn-string #" ")]
      (str japanese "\t" grammar "\t" (apply str definition)))))

(defn clean-text [text-string]
  (->> (split text-string #"\n")
       merge-definitions
       remove-extra
       sort
       (map (comp create-csv-pairs drop-numbering))))

(defn to-csv-file [pdf-location]
  (let [location (first (split pdf-location #"\\."))]
    (str location ".csv")))

(defn create-anki-deck [pdf-location]
  (->> (text/extract pdf-location)
       clean-text
       (join "\n")
       (spit (to-csv-file pdf-location))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (str "Parsing all files: " (join " " args)))
  (dorun (map create-anki-deck args))
  (println "Finished. Please enjoy!"))
