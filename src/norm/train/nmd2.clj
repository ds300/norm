(ns norm.train.nmd2
  (:require [norm.config :as config]
            [norm.io :as io]
            [norm.data :as data]))

; misc utils
(defn word-tokens [^String text]
  (map first (re-seq #"((?<= )|(?<=^))[a-z][a-z\-']*" (.toLowerCase text))))

(defn pmap-chunked [n f coll]
  (apply concat
    (pmap #(map f %) (partition-all n coll))))

(def ^:dynamic *min-freq*)
(def ^:dynamic *min-length*)
(def ^:dynamic *lex-dist*)
(def ^:dynamic *phon-dist*)
(def ^:dynamic *post-rank-cutoff*)
(def ^:dynamic *counter*)
(def ^:dynamic *n-gram-order*)
(def ^:dynamic *window-size*)

(def IV_IDS (atom {}))
(defn- iv-id [word] (@IV_IDS word))

(def IV_WORDS (atom (trie/trie)))
(def OOV_WORDS (atom {}))

(def CTX_REL (atom #{})) ; set of words we need to extract context for

(def CTX_IDS (ref {})) ; ids of context features
(def CTX_FREQS (ref [])) ; frequencies of context features



(defn- load-data! []
  ; give IV words unique IDs
  (io/doing-done "Assigning IV IDs"
    (swap! IV_IDS into (map vector (.words data/DICT) (range))))
  ; get unigram frequencies from twt
  (let [in (io/prog-reader (data/get-path :twt))]
    ; iterate over frequencies and add words into tries
    (println "Deriving unigram frequencies from" (data/get-path :twt))
    (doseq [[word freq] (progress/monitor
                          [#(str "\t" (.progress in))]
                          (frequencies (mapcat word-tokens (io/lines-in in))))]
      (cond
      ; when word is IV, just add it into IV_WORDS
        (iv-id word) (swap! IV_WORDS conj [word freq (atom {})])
      ; when word is OOV, only include it if *min-freq* and *min-length*
        :else (when (and (<= *min-freq* freq) (<= *min-length* (count word)))
                (swap! OOV_WORDS conj [word (atom {})]))))))


;;;; GENERATING CONFUSIONS SETS ;;;;

(defn confusion-set [word]
  ; only exctract IV words that have been observed in twt
  (let [iv_deref @IV_WORDS ; deref this to save processings
        dm_words (filter iv_deref (mapcat data/DM-DICT (trie/find-within data/DM-DICT *phon-dist*)))
        iv_words (trie/find-within iv_deref *lex-dist*)
        sorted_words (sort-by #(- (trie/freq iv_deref %)) (concat dm_words iv_words))
        n (int (* *post-rank-cutoff* (/ (count words) 100)))]
    (take n sorted_words)))


(defn- generate-confusion-set [[word [_ data_atom]]]
  (let [cs (confusion-set word)]
    (swap! data_atom conj [:confusion-set cs])
    (swap! CTX_REL into (conj word cs))
    (swap! *counter* inc)))

(defn- generate-confusion-sets! []
  (println "Generating confusion sets for OOV words")
  (binding [*counter* (atom 0)]
    (progress/monitor [#(str "\t" @*counter* " words processed")]
      (pmap-chunked 100 generate-confusion-set @OOV_WORDS))))


;;;; EXTRACTING CONTEXTUAL FEATURES ;;;;

(defn- ctx-id [ctx]
  (or
    ; try to read the id without setting up a transaction first
    (get-in @CTX_IDS ctx)
    ; it's not in there so set up a transaction
    (dosync
      ; it might have been inserted by some other thread before
      ; this transaction got set up so check that first
      (or
        (get-in @CTX_IDS ctx)
        ; its still not in there, so insert it and make a new
        ; counter atom in CTX_FREQS, then return the id
        (do
          (commute CTX_FREQS conj (atom 0))
          (let [id (count @CTX_IDS)]
            (alter CTX_IDS assoc-in ctx (count @CTX_IDS))
            id))))))

(defn- store-context-features [[word ctxs]]
  (doseq [ctxid (map ctx-id ctxs)]
    (swap! (@CTX_FREQS ctxid) inc)
    (if-let [data_atom (or (@IV_WORDS word) (@OOV_WORDS word))]
      (swap! data_atom update-in [:context ctxid] (fnil inc 0)))))

(defn- context-left [grams i]
  (map
    into
    (map (comp vector -) (rest (range)))
    (reverse (words/context-left grams *window-size* (+ i 1 (- *n-gram-order*))))))

(defn- context-right [grams i]
  (map
    into
    (map vector (rest (range)))
    (words/context-right grams *window-size* i)))

(defn- context [grams i]
  (filter (partial every? identity)
    (concat
      (context-left grams i)
      (context-right grams i))))

(defn- extract-context [line]
  (let [tokens (words/tokenise (.toLowerCase line))
        ngrams (words/ngrams *n-gram-order* (map iv-id tokens))]
    (doseq [[word i] (map vector tokens (range))]
      ; only extract context for relevant words
      (when (@CTX_REL word)
        (store-context-features word (context ngrams i))))))

(defn- extract-all-context! []
  (println "Extracting contextual features...")
  (let [in (io/prog-reader (data/get-path :twt))]
    (progress/monitor [#(str "\t" (.progress in))]
      (pmap-chunked 200 extract-context (io/lines-in in)))))


(defn train []
  (binding [*min-freq* (config/opt :train :nmd :min-freq)
            *min-length* (config/opt :train :nmd :min-length)
            *lex-dist* (config/opt :train :nmd :lex-dist)
            *phon-dist* (config/opt :train :nmd :phon-dist)
            *post-rank-cutoff* (config/opt :train :nmd :post-rank-cutoff)
            *n-gram-order* (config/opt :train :nmd :n-gram-order)
            *window-size* (config/opt :train :nmd :window-size)]
  (load-data!)
  (generate-confusion-sets!)
  (extract-all-context!)))