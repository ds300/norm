(ns norm.train.nmd
  (:import [uk.ac.susx.mlcl.lib.collect SparseDoubleVector]
           [uk.ac.susx.mlcl.byblo.measures
                         AbstractMIProximity
                         AbstractProximity
                         Confusion
                         Cosine
                         CosineMi
                         CrMi
                         Dice
                         DiceMi
                         Hindle
                         Jaccard
                         JaccardMi
                         Jensen
                         Lee
                         Lin
                         Lp
                         Overlap
                         RecallMi
                         Tanimoto])
  (:require [norm.config :as config]
            [norm.io :as io]
            [norm.data :as data]
            [norm.trie :as trie]
            [norm.utils :as utils]
            [norm.progress :as progress]
            [norm.words :as words]))

(set! *warn-on-reflection* true)


(defn inc-freq! [transient_map k]
  "Takes a transient map whose values are numbers and a key k,
  and increments the number specified by k, or makes it 1 if not
  present."
  (assoc! transient_map k (inc (transient_map k 0))))

(defn stratify-corpus-words!
  "takes a dictionary, a predicate of type (oov_word count) -> Bool,
  and an input reader, and returns the words found in reader in two forms.
  The first is a list of all OOV words which the predicate returned true for,
  the second is a frequency-augmented trie of all the IV words observed."
  [^norm.jvm.Trie DICT oov_predicate in]
  (loop [[word & remaining] (->> in
                              line-seq
                              (filter not-empty)
                              (utils/pmapcat-chunked 500 words/word-tokenise))
          iv_acc (transient {})
          oov_acc (transient {})]
    (if word
      (if (.contains DICT word)
          (recur remaining (inc-freq! iv_acc word) oov_acc)
          (recur remaining iv_acc (inc-freq! oov_acc word)))
      ; else return seq of oov words and trie of iv words
      [
        (map first (filter oov_predicate (persistent! oov_acc)))
        (trie/trie (persistent! iv_acc))
      ])))


(defn get-confusion-set
  "Takes a bunch of parameters and a word, and returns a confusion set
  for the word."
  [dm-dict lex_dist phon_dist cutoff-percent iv_trie word]
  (let [cs (words/raw-confusion-set iv_trie dm-dict lex_dist phon_dist word)
        n (int (-> (count cs) (/ 100) (* cutoff-percent)))
        freq #(- (.tfreq iv_trie %))]
    (take n (sort-by freq cs))))

(defn generate-confusion-sets!
  "takes a counter, a function of type word -> confusion_set, and 
  a list of words. Returns a map from words to their confusion sets.
  Increments the counter for every word processed."
  [counter* get-cs oov_words]
  (let [doit (fn [oov_word] (counter* 1) [oov_word (get-cs oov_word)])]
    (into {} (pmap doit oov_words))))

(defn get-context-accumulator-map
  "takes a map from words to confusion sets, and returns a map
  from all words in all confusion sets plus those which are keys
  in the input map to atoms containing empty maps."
  [oov_cs_map]
  (into {} 
    (map vector 
      (into #{} (flatten (seq oov_cs_map)))
      (repeatedly #(atom {})))))


(defn feature-id! 
  "Takes a ref containing a map of feature ids, a ref containing
  a vector of feature frequencies, and a feature. Returns
  a unique id for the feature. If the feature has not been
  seen before, it adds an entry in feature_ids* and an atom containing
  the value 0 in feature_freqs*."
  [feature_ids* feature_freqs* feature]
  (or
    ; try to read the id without setting up a transaction first
    (get-in @feature_ids* feature)
    ; it's not in there so set up a transaction
    (dosync
      ; it might have been inserted by some other thread before
      ; this transaction got set up so check that first
      (or
        (get-in @feature_ids* feature)
        ; its still not in there, so insert it and make a new
        ; counter atom in feature_freqs*, then return the id
        (do
          (commute feature_freqs* conj (atom 0))
          (let [id (count @feature_ids*)]
            (alter feature_ids* assoc-in feature (count @feature_ids*))
            id))))))


(defn context-left
  "returns indexed left n-gram context for the token at position i."
  [n_gram_order window_size grams i]
  (map into
    (map (comp vector -) (rest (range)))
    (reverse (words/context-left grams window_size (+ i 1 (- n_gram_order))))))

(defn context-right
  "returns indexed right n-gram context for the token at position i."
  [window_size grams i]
  (map into
    (map vector (rest (range)))
    (words/context-right grams window_size i)))

(defn context
  "returns all indexed context for the token at position i"
  [n_gram_order window_size grams i]
  (concat
    (context-left n_gram_order window_size grams i)
    (context-right window_size grams i)))

(def no-nils? (partial every? (comp not nil?)))

(defn store-context!
  "takes a line (presumably a tweet of some sort) and extracts contextual features
  which get stored in ctx-acc*. Also their frequencies get stored in feature-freqs*"
  [n_gram_order window_size iv_ids ctx-acc* feature-freqs* feature-id* line]
  (let [tokens (words/tokenise (.toLowerCase line))
        ngrams (words/n-grams n_gram_order (map iv_ids tokens))]
    (doseq [[word i] (map vector tokens (range))]
      ; only extract context for relevant words
      (when (ctx-acc* word)
        (doseq [feature (->> (context n_gram_order window_size ngrams i)
                          (filter no-nils?) ; make sure all context words are IV
                          (map feature-id*))]
          (swap! (feature-freqs* feature) inc)
          (swap! (ctx-acc* word) update-in [feature] (fnil inc 0)))))))

(defn extract-all-context!
  "Extracts all contextual features from the lines in in for all oov and iv
  words in oov_cs_map"
  [n_gram_order window_size iv_ids oov_cs_map in]
  (let [ctx-acc* (get-context-accumulator-map oov_cs_map)
        feature-freqs* (ref [])
        id!_  (partial feature-id! (ref {}) feature-freqs*)
        store!_ (partial store-context! n_gram_order window_size iv_ids ctx-acc* feature-freqs* id!_)]
    ;; do the actual computation
    (dorun (utils/pmap-chunked 20 store!_ (filter not-empty (line-seq in))))
    ;; return still-atmoised data structures
    [ctx-acc* @feature-freqs*]))

(defn to-sdv
  "converts a frequency distribution to a SparseDoubleVector
  with the specified cardinality"
  [freq_dist cardinality]
  (if (zero? (count freq_dist))
    (SparseDoubleVector. cardinality 0)
    (let [^double sum  (reduce + (vals freq_dist))
          ks   (int-array (sort (keys freq_dist)))
          vs   (double-array (map freq_dist ks))
          card cardinality
          size (count ks)]
      (SparseDoubleVector. ks vs card size))))

(defn make-sdvs!
  "converts the frequency distributions stored in all_context
  to SparseDoubleVector objects with the specified cardinality.
  increases counter for each sdv created."
  [counter* cardinality all_context*]
  (doall
    (pmap #(do (swap! % to-sdv cardinality) (counter* 1))
      (vals all_context*))))



(defn get-pair
  "Takes a distributional proximity measure, a map of SparseDoubleVector
  objects, an oov word and it's confusion set, and returns a tuple of the oov 
  word and the item from its confusion set whose distributional proximity to
  the oov word is lowest (i.e. closest)"
  [measure all_context* oov_word confusion_set]
  (let [oov_context @(all_context* oov_word)
        left_result (.left measure oov_context)
        candidates  (for [iv_word confusion_set]
                      (let [iv_context@(all_context* iv_word)
                            right_result (.right measure iv_context)
                            shared_result (.shared ^AbstractProximity measure oov_context iv_context)
                            combined (.combine measure shared_result left_result right_result)]
                        [iv_word combined]))
        [top_candidate _] (first (sort-by second candidates))]
    (when top_candidate
      [oov_word top_candidate])))

(defn get-pairs!
  "makes distributionally similar pairs for all keys of oov_cs_map.
  increments counter for each word processed."
  [counter* oov_cs_map all_context* measure]
  (doall
    (filter identity
      (pmap 
        (fn [[oov_word confusion_set]]
          (counter* 1)
          (get-pair measure all_context* oov_word confusion_set))
        oov_cs_map))))

(defn rank-pairs
  "Sorts a collection of pairs of strings according to their
  similarity as defined by the string subsequence kernel."
  [pairs]
  (let [kernel (cc.mallet.types.StringKernel.)
        ssk (fn [^String s ^String t] (.K kernel s t))]
    (map rest
      (sort
        (for [[oov iv] pairs]
          [(- (ssk oov iv)) oov iv])))))

(defn train! []
  (data/verify-readable! :twt :dict :dm-dict)

  (data/load-and-bind [:dict :dm-dict]
    (let [iv_ids           (into {} (map vector (.words data/DICT) (range)))
          twt_path         (data/get-path :twt)
          lex_dist         (config/opt :confusion-sets :lex-dist)
          phon_dist        (config/opt :confusion-sets :phon-dist)
          post_rank_cutoff (config/opt :train :nmd :post-rank-cutoff)
          n_gram_order     (config/opt :train :nmd :n-gram-order)
          window_size      (config/opt :train :nmd :window-size)
          pair_rank_cutoff (config/opt :train :nmd :pair-rank-cutoff)
          min_freq         (config/opt :train :nmd :min-freq)
          min_length       (config/opt :train :nmd :min-length)
          measure_type     (config/opt :train :nmd :measure)
          oov_predicate    (fn [[word freq]]
                             (and
                               (<= min_freq freq)
                               (<= min_length (count word))))]

      (utils/with-atoms [oov_words iv_trie oov_cs_map all_context* feature_freqs pairs]
        ; get our words from the corpus in the relevat formats
        (io/open [:r in twt_path]
          (println "Counting words...")
          (progress/monitor [#(str "\t" (.progress in))]
            (utils/assign! [oov_words iv_trie] (stratify-corpus-words! data/DICT oov_predicate in))))

        (let [counter* (utils/counter)]
          (println "Generating confusion sets...")
          (progress/monitor [#(str "\t" (counter*))]
            (reset! oov_cs_map
              (generate-confusion-sets!
                counter*
                (partial get-confusion-set data/DM-DICT lex_dist phon_dist post_rank_cutoff @iv_trie)
                @oov_words))))

        ;; we don't need these anymore
        (reset! oov_words nil)
        (reset! iv_trie nil)
        
        (io/open [:r in twt_path]
          (println "Extracting all contextual features...")
          (progress/monitor [#(str "\t" (.progress in)) 2000]
            (utils/assign! [all_context* feature_freqs]
              (extract-all-context! n_gram_order window_size iv_ids @oov_cs_map in))))

        (let [counter* (utils/counter)]
          (println "Converting feature frequency distributions to sparse vectors...")
          (progress/monitor [#(str "\t" (counter*))]
            (make-sdvs! counter* (count @feature_freqs) @all_context*)))

        (let [counter* (utils/counter)
              measure (eval (read-string (str "(uk.ac.susx.mlcl.byblo.measures."measure_type".)")))]

          (when (instance? AbstractMIProximity measure)
            (reset! feature_freqs (double-array (map deref @feature_freqs)))
            (doto measure
              (.setFeatureFrequencies @feature_freqs)
              (.setFeatureFrequencySum (reduce + @feature_freqs))))

          (println "Deriving contextually similar pairs...")
          (progress/monitor [#(str "\t" (counter*))]
            (reset! pairs (get-pairs! counter* @oov_cs_map @all_context* measure))))

        ;; we don't need these anymore
        (reset! all_context* nil)
        (reset! oov_cs_map nil)
        (reset! feature_freqs nil)

        (println "Ranking pairs, applying cutoff, and writing to disk...")
        (io/open [:w out io/OUT_PATH]
          (io/spit-tsv out (take pair_rank_cutoff (rank-pairs @pairs))))))))

