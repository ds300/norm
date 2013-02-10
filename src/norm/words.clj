(ns norm.words
  (:require [norm.data :as data]))

(def double-metaphone
  (let [dm (org.apache.commons.codec.language.DoubleMetaphone.)]
    (fn [^String string]
      (.doubleMetaphone dm
        (clojure.string/replace string #"(.)\1\1+" "$1$1")))))

(defn confusion-set [word lexi-d phon-d])

(defn tokenise [^String text]
  (into [] (cmu.arktweetnlp.Twokenize/tokenizeRawTweetText text)))



(defn remove-punct-repetition [^String line]
  (clojure.string/replace line #"(\p{Punct})\1\1\1+" "$1$1$1"))

(defn n-grams
  "returns a vector of the n-grams in the given sequence"
  [n sequence]
  (mapv vec (partition n 1 sequence)))

(defn numeric-label
  "labels the items in sequence (must be stringifiable).

  e.g. with   label=\"hello:\"
       and    sequence=[[\"steve\" \"wilson\"] [\"andy\" \"serkis\"]]
  you get:
    (\"hello:1:steve wilson\" \"hello:2:andy serkis\")"
  [^String label ^clojure.lang.ISeq sequence]
  (map
    (fn [i gram] (str label i ":" (apply str (interpose " " gram))))
    (rest (range))
    sequence))

(defn context-left [coll window_size i]
  (subvec coll
    (max 0 (- i window_size))
    (max 0 i)))

(defn context-right [coll window_size i]
  (subvec coll
    (min (+ i 1) (count coll))
    (min (+ i 1 window_size) (count coll))))


(defn ngram-context-left [grams n window_size i]
  (context-left grams window_size (+ i 1 (- n))))

(defn ngram-context-right [grams n window_size i]
  (context-right grams window_size i))


(defn ngram-context-labeled
  "extracts stringified versions of context for the given index"
  [label grams n window_size i]
  (concat
    (numeric-label (str label "-") (reverse (ngram-context-left grams n window_size i)))
    (numeric-label (str label "+") (ngram-context-right grams n window_size i))))
