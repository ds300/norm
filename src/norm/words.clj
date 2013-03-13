(ns norm.words
  (:import [norm.jvm StringComparators])
  (:require [norm.trie :as trie]
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
  (subvec coll
    (max 0 (- i window_size))
    (max 0 i)))

(defn context-right [coll window_size i]
  (subvec coll
    (min (+ i 1) (count coll))
    (min (+ i 1 window_size) (count coll))))

(defn indexed-context [coll window_size i]
  (let [l (context-left coll window_size i)
        r (context-right coll window_size i)]

    (into (mapv vector
            (map - (rest (range)))
            (reverse l))
          (map vector
            (rest (range))
            r))))

(defn raw-confusion-set [dict dm-dict lex-dist phon-dist word]
  (into []
    (concat
      (mapcat dm-dict (trie/find-within dm-dict (double-metaphone word) phon-dist))
      (trie/find-within dict word lex-dist))))

; TODO: generalise this for window size maybe?
(defn lm-ranked-confusion-set [lm get-cs tokens i]
  (let [cs            (get-cs (tokens i))
        ranks         (atom (into {} (map vector cs (repeat 0))))
        lctx          (context-left tokens 2 i)
        rctx          (context-right tokens 2 i)
        ; sort confusion set by the scores of sentences returned by lsfn
        ; in descending order of goodness
        sort-         (fn [lsfn]
                        (map last
                          (sort
                            (for [c cs]
                              [(- (.scoreSentence lm (lsfn c))) c]))))
        ; get [word rank] pairs for the confusion set
        rank          (fn [lsfn]
                        (map vector
                          (sort- lsfn)
                          (range)))
        ; update the ranks atom with cs ranked by lsfn
        update-ranks! (fn [lsfn]
                       (swap! ranks (partial merge-with +) (into {} (rank lsfn))))]
    ; (when (seq lctx)
    ;   (update-ranks! #(conj lctx %)))
    ; (when (seq rctx)
    ;   (update-ranks! #(cons % (seq rctx))))
    (update-ranks! #(filter identity [(last lctx) % (first rctx)]))
    (map first (sort-by last @ranks))))
