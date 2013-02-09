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