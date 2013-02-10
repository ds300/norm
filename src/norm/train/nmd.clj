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

(def CTX_IDS (atom (trie/trie)))
(def CTX_FREQS (atom []))

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

(defn fast-freqs [words]
  (let [a (agent {})] (doseq [word words] (send a update-in [word] (fnil inc 0)))))

(defn setup-tries! []
  (swap! IV trie/combine data/DICT)
  (let [in (io/prog-reader (data/get-path :twt))
        min_freq (config/opt :train :nmd :freq-cutoff)]
    (doseq [[word freq] (progress/monitor [#(str "Deriving unigram frequencies from twt ... " (.progress in))]
                          (fast-freqs (mapcat word-tokens (io/lines-in in))))]
      (if (.contains data/DICT word)
        (swap! IV conj [word freq (atom {})])
        (when (<= min_freq freq)
          (swap! OOV conj [word freq (atom {})]))))))

(def COUNTER (atom 0))

(defn generate-confusion-sets! []
  (reset! COUNTER 0)
  (progress/monitor [#(str "Generating confusion sets ... " @COUNTER " oov words processed.")]
    (doseq [[oov_word [_ data]] @OOV]
      (swap! data conj [:confusion-set (nmd-confusion-set oov_word)])
      (swap! COUNTER inc))))

(defn deref-confusion-set [word]
  (@(@OOV word) :confusion-set))

(defn store-context [dump word context]
  (doseq [ctx context]
    (let [ctxid (or (.tfreq @CTX_IDS ctx)
                    (do (swap! CTX_FREQS conj (agent 0)) 
                        (dec (count (swap! @CTX_IDS #(conj % [ctx (count %)]))))))]
      (send (@CTX_FREQS ctxid) inc)
      (swap! (@dump word) update-in [:context ctxid] (fnil inc 0)))))

(def ^:dynamic N_GRAM_ORDER)
(def ^:dynamic WINDOW_SIZE)

(defn extract-context [line]
  (let [tokens (words/tokenise (.toLowerCase line))
        n_gram_order (config/opt :train :nmd :n-gram-order)
        window_size (config/opt :train :nmd :window-size)
        ngrams (words/n-grams n_gram_order tokens)
        get-context (partial words/ngram-context-labeled "f" ngrams n_gram_order window_size)]
    (doseq [[index word] (map vector (range) tokens)]
      (cond
        (.contains data/DICT word) (store-context IV word (get-context index))
        (.contains @OOV word) (store-context OOV word (get-context index)))))
  (swap! COUNTER inc))

(defn extract-all-context! []
  (let [in (io/prog-reader (data/get-path :twt))]
    (binding [N_GRAM_ORDER (config/opt :train :nmd :n-gram-order)
              WINDOW_SIZE  (config/opt :train :nmd :window-size)]
      (progress/monitor [#(str "Extracting all contextual features ... " (.progress in))]
        (dorun 
          (pmap
            #(dorun (map extract-context %))
            (partition-all 500 extract-context)))))))

(defn train []
  (setup-tries!)
  (generate-confusion-sets!)
  )