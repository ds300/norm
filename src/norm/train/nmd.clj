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


(defn count-corpus-words!
  "counts the words in the given input streams.
  returns a seq of [word freq] pairs"
  [in]
  (let [word_freqs* (utils/atomised-map-counter)
        process-tweet* (fn [line]
                         (doseq [word (words/word-tokenise line)]
                           (word_freqs* word 1)))]
    (->> in
      line-seq
      (filter not-empty)
      (utils/pmapall-chunked 500 process-tweet*)
      dorun)

    (for [[word freq_atom] (seq (word_freqs*))]
      [word @freq_atom])))

(defn stratify-counted-words!
  "Takes a dictionary, an oov_predicate which should accept a [word, freq] pair,
  a counter for progress tracking, and a seq of [word, freq] pairs. returns
  a [iv_trie, oov_words_list] pair"
  [^norm.jvm.Trie DICT oov_predicate counter* word_freqs]
  (loop [[[word freq :as wf] & more] word_freqs
         iv_trie     (trie/trie)
         oov_words   (transient [])]
    (counter* 1)
    (if word
      (if (.contains DICT word)
        (recur more (conj iv_trie wf) oov_words)
        (recur more iv_trie
          (if (oov_predicate wf)
            (conj! oov_words word)
            oov_words)))
      [(persistent! oov_words) iv_trie])))

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
    (into {} (utils/pmapall doit oov_words))))

(defn get-context-accumulator-map
  "takes a map from words to confusion sets, and returns a map
  from all words in all confusion sets plus those which are keys
  in the input map to atoms containing empty maps."
  [oov_cs_map]
  (into {} 
    (map vector 
      (into #{} (flatten (seq oov_cs_map)))
      (repeatedly #(atom {})))))

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
      (when (ctx-acc* word)
        (doseq [fid (->> (context n_gram_order window_size ngrams i)
                      (filter no-nils?)
                      (map (comp feature-id* int-array)))]
          (swap! (@feature-freqs* fid) inc)
          (swap! (ctx-acc* word) update-in [fid] (fnil inc 0)))))))

(defn extract-all-context!
  "Extracts all contextual features from the lines in in for all oov and iv
  words in oov_cs_map"
  [n_gram_order window_size iv_ids ctx-acc* in]
  (let [feature-freqs* (atom [])
        feature-id*    (utils/unique-id-getter 0 (fn [n]
                                                   (swap! feature-freqs* #(if (>= n (count %))
                                                                            (conj % (atom 0))
                                                                            %))))
        handle-tweet!  (fn [line]
                         (store-context! n_gram_order window_size iv_ids
                           ctx-acc* feature-freqs* feature-id* line))]
    ;; do the actual computation
    (dorun (utils/pmapall-chunked 500 handle-tweet! (filter not-empty (line-seq in))))
    ;; return still-atmoised data structures
    @feature-freqs*)

  )

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
  (dorun
    (utils/pmapall #(do (swap! % to-sdv cardinality) (counter* 1))
      (vals all_context*))))



(defn get-pair
  "Takes a distributional proximity measure, a map of SparseDoubleVector
  objects, an oov word and it's confusion set, and returns a tuple of the oov 
  word and the item from its confusion set whose distributional proximity to
  the oov word is lowest (i.e. closest)"
  [^AbstractProximity measure all_context* oov_word confusion_set]
  (let [oov_context @(all_context* oov_word)
        left_result (.left measure oov_context)
        candidates  (for [iv_word confusion_set]
                      (let [iv_context @(all_context* iv_word)
                            right_result (.right measure iv_context)
                            shared_result (.shared measure oov_context iv_context)
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
      (utils/pmapall
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

      (utils/with-atoms [counts oov_words iv_trie oov_cs_map all_context* feature_freqs pairs]
        ; get our words from the corpus in the relevat formats
        (io/open [:r in twt_path]
          (println "Counting words...")
          (progress/monitor [#(str "\t" (.progress in))]
            (reset! counts (count-corpus-words! in))))

        (let [counter* (utils/counter)]
          (println "Stratifying counted words...")
          (progress/monitor [#(str "\t" (counter*))]
            (utils/assign! [oov_words iv_trie] (stratify-counted-words! data/DICT oov_predicate counter* @counts))))

        ; we don't need this anymore
        (reset! counts nil)

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

        (io/doing-done "Generating context accumulator map"
          (reset! all_context* (get-context-accumulator-map @oov_cs_map)))
        
        (io/open [:r in twt_path]
          (println "Extracting all contextual features...")
          (progress/monitor [#(str "\t" (.progress in)) 2000]
            (reset! feature_freqs
              (extract-all-context! n_gram_order window_size iv_ids @all_context* in))))

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


        (io/open [:w out io/OUT_PATH]
          (io/doing-done "Ranking pairs, applying cutoff, and writing to disk..."
            (io/spit-tsv out (take pair_rank_cutoff (rank-pairs @pairs)))))))))

