(ns norm.words
  (:require [norm.trie :as trie]
            [clojure.string :as str]))

(def double-metaphone
  (let [dm (org.apache.commons.codec.language.DoubleMetaphone.)]
    (fn [^String string]
      (.doubleMetaphone dm
        (str/replace string #"(.)\1\1+" "$1$1")))))

(defn word-tokenise [^String text]
  (map first (re-seq #"((?<= )|(?<=^))[a-z][a-z\-']*" (.toLowerCase text))))

(defn tokenise [^String text]
  (into [] (cmu.arktweetnlp.Twokenize/tokenizeRawTweetText text)))

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


(defn ngram-context-left [grams n window_size i]
  (context-left grams window_size (+ i 1 (- n))))

(defn ngram-context-right [grams n window_size i]
  (context-right grams window_size i))


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
    (when (seq lctx)
      (update-ranks! #(conj lctx %)))
    (when (seq rctx)
      (update-ranks! #(cons % (seq rctx))))
    (update-ranks! #(filter identity [(last lctx) % (first rctx)]))
    (map first (sort-by last @ranks))))
