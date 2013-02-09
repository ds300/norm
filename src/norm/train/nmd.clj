(ns norm.train.nmd
  (:require [norm.data :as data]
            [norm.config :as config]
            [norm.io :as io]
            [norm.progress :as progress]
            [norm.trie :as trie]
            [norm.words :as words]))


(defn word-tokens [^String text]
  (re-seq #"[a-z][a-z\-']*" (.toLowerCase text)))

(def IV (atom (trie/trie)))
(def OOV (atom (trie/trie)))

(def CTXIDS (atom (trie/trie)))

(defn nmd-confusion-set [word]
  (let [prc (config/opt :train :nmd :post-rank-cutoff)
        lex_dist (config/opt :confusion-sets :lex-dist)
        phon_dist (config/opt :confusion-sets :phon-dist)
        words (sort-by #(- (trie/freq @IV %))
                (concat
                  (into [] (trie/find-within data/DICT word lex_dist))
                  (apply concat
                    (map data/DM-DICT
                      (trie/find-within data/DM-DICT (words/double-metaphone word) phon_dist)))))
        n (int (* prc (/ (count words) 100)))]
    (take n words)))

(defn setup-tries! []
  (send IV trie/combine data/DICT)
  (let [in (io/prog-reader (data/get-path :twt))
        min_freq (config/opt :train :nmd :freq-cutoff)]
    (doseq [[word freq] (progress/monitor [#(str "Deriving unigram frequencies from twt ... " (.progress in) "%")]
                          (frequencies (mapcat word-tokens (io/lines-in in))))]
      (if (.contains data/DICT word)
        (swap! IV conj [word freq (atom {})])
        (when (<= min_freq freq)
          (swap! OOV conj [word freq (atom {})]))))))

(defn generate-confusion-sets! []
  (progress/monitor [#(str "Generating confusion sets")]
    (doseq [[oov_word [_ data]] @OOV]
      (swap! data conj [:confusion-set (nmd-confusion-set oov_word)]))))

(defn get-confusion-set [word]
  (@(@OOV word) :confusion-set))

(defn extract-context [line]
  (let [tokens (words/tokenise (.toLowerCase line))]
    ))

(defn extract-all-context! []
  )

(defn train []
  (setup-tries!)
  (generate-confusion-sets!)
  )