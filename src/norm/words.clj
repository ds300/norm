(ns norm.words
  (:import [norm.jvm StringComparators])
  (:require [norm.trie :as trie]
            [norm.utils :as utils]
            [clojure.string :as str]))

(defn levenshtein [^String a ^String b]
  (StringComparators/levenshteinDistance a b))

(defn common-prefix-length [^String a ^String b]
  (StringComparators/commonPrefixLength a b))

(defn common-suffix-length [^String a ^String b]
  (StringComparators/commonSuffixLength a b))

(defn longest-common-subsequence [^String a ^String b]
  (StringComparators/longestCommonSubsequence a b))

(let [kernel (cc.mallet.types.StringKernel.)]
  (defn ssk [^String a ^String b]
    (.K kernel a b)))

(defn remove-repetition [s]
  (-> s
    (str/replace #"(.)\1\1\1+" "$1$1$1")
    (str/replace #"(..)\1\1+" "$1$1")))

(def double-metaphone
  (let [dm (org.apache.commons.codec.language.DoubleMetaphone.)]
    (fn [^String string]
      (.doubleMetaphone dm
        (remove-repetition string)))))

(defn word-tokenise [^String text]
  (->> text
    (.toLowerCase)
    (remove-repetition)
    (re-seq #"((?<= )|(?<=^))[a-z][a-z\-']*")
    (map first)))

(defn tokenise [^String text]
  (->> text
    remove-repetition
    cmu.arktweetnlp.Twokenize/tokenizeRawTweetText
    (into [])))

(def tokenise-lower (comp tokenise str/lower-case))

(defn remove-punct-repetition [^String line]
  (str/replace line #"(\p{Punct})\1\1\1+" "$1$1$1"))

(defn n-grams
  "Returns a vector of the n-grams in the given sequence.
   For lazy n-grams just do (partition n 1 coll) or map vec over that
   if you need your grams as vectors."
  [n sequence]
  (mapv vec (partition n 1 sequence)))

(defn context-left [coll window_size i]
  (when-not (vector? coll)
    (throw
      (IllegalArgumentException. "context only works for vectors, yeah?")))
  (if (seq coll)
    (subvec coll
      (max 0 (- i window_size))
      (max 0 i))
    []))

(defn context-right [coll window_size i]
  (when-not (vector? coll)
    (throw
      (IllegalArgumentException. "context only works for vectors, yeah?")))
  (if (seq coll)
    (subvec coll
      (min (+ i 1) (count coll))
      (min (+ i 1 window_size) (count coll)))
    []))

(defn indexed-context [coll window_size i]
  (let [l (context-left coll window_size i)
        r (context-right coll window_size i)]

    (into (vec
            (reverse
              (map vector
                (map - (rest (range)))
                (reverse l))))
          (map vector
            (rest (range))
            r))))

(defn raw-confusion-set [dict dm-dict lex-dist phon-dist word]
  (into []
    (concat
      (mapcat dm-dict (trie/find-within dm-dict (double-metaphone word) phon-dist))
      (trie/find-within dict word lex-dist))))


(defn lm-ranked-confusion-set [lm get-cs tokens i]
  (let [cs            (get-cs (nth tokens i))
        lctx          (context-left tokens 1 i)
        rctx          (context-right tokens 1 i)
        ; sort confusion set by the scores of sentences returned by lsfn
        ; in descending order of goodness
        get-trigram   (fn [w] (into (conj lctx w) rctx))]
    (->> cs
      (utils/mapply (comp - lm get-trigram))
      (sort-by last)
      (map first))))
