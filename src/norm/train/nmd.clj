(ns norm.train.nmd
  (:require [norm.data :as data]
            [norm.config :as config]
            [norm.io :as io]
            [norm.progress :as progress]
            [norm.trie :as trie]
            [norm.words :as words])
  (:import [uk.ac.susx.mlcl.lib.collect SparseDoubleVector]
           [uk.ac.susx.mlcl.byblo.measures
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
                         Tanimoto]))


(defn word-tokens [^String text]
  (map first (re-seq #"((?<= )|(?<=^))[a-z][a-z\-']*" (.toLowerCase text))))

(defn pmap-chunked [n f coll]
  (apply concat
    (pmap #(map f %) (partition-all n coll))))

(def IV (atom (trie/trie)))
(def OOV (atom (trie/trie)))

(def CTX_REL (agent #{})) ; hash set of all words we will extract context for

(def CTX_IDS (atom (trie/trie)))

(def CTX_FREQS (atom [])) ; list of agents 

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
  (swap! IV trie/combine data/DICT)
  (println "deriving unigram frequencies from" (.getAbsolutePath (java.io.File. (data/get-path :twt))))
  (let [in (io/prog-reader (data/get-path :twt))
        min_freq (config/opt :train :nmd :freq-cutoff)
        min_length (config/opt :train :nmd :min-length)]
    (doseq [[word freq] (progress/monitor [#(str "    Working ... " (.progress in))]
                          (frequencies (mapcat word-tokens (io/lines-in in))))]
      (if (.contains data/DICT word)
        (swap! IV conj [word freq (atom {})])
        (when (<= min_freq freq)
          (swap! OOV conj [word freq (atom {})])))))
  (send CTX_REL into (.words @OOV))
  (await CTX_REL))

(def COUNTER (atom 0))

(defn generate-confusion-sets! []
  (reset! COUNTER 0)
  (progress/monitor [#(str "Generating confusion sets ... " @COUNTER " oov words processed.")]
    (doseq [[oov_word [_ data]] @OOV]
      (let [cs (nmd-confusion-set oov_word)]
        (swap! data conj [:confusion-set cs])
        (send CTX_REL into cs))
      
      (swap! COUNTER inc))))

(defn deref-confusion-set [word]
  (@(@OOV word) :confusion-set))

(defn store-context [dump word context]
  (doseq [feature context]
    (let [ctxid (or (@CTX_IDS feature)
                    (do (swap! CTX_FREQS conj (agent 0)) 
                        (dec (count (swap! CTX_IDS #(conj % [feature 0 (count %)]))))))]
      (send (@CTX_FREQS ctxid) inc)
      (swap! (@dump word) update-in [:context ctxid] (fnil inc 0)))))


(def ^:dynamic N_GRAM_ORDER)
(def ^:dynamic WINDOW_SIZE)

(defn extract-context [line]
  (let [tokens (words/tokenise (.toLowerCase line))
        ngrams (words/n-grams N_GRAM_ORDER tokens) 
        get-context (partial words/ngram-context-labeled "f" ngrams N_GRAM_ORDER WINDOW_SIZE)]
    (doseq [[index word] (map vector (range) tokens)]
      (when (@CTX_REL word)
        (cond
          (.contains data/DICT word) (store-context IV word (get-context index))
          (.contains @OOV word) (store-context OOV word (get-context index)))))))

(defn extract-all-context! []
  (let [in (io/prog-reader (data/get-path :twt))]
    (binding [N_GRAM_ORDER (config/opt :train :nmd :n-gram-order)
              WINDOW_SIZE  (config/opt :train :nmd :window-size)]
      (progress/monitor [#(str "Extracting all contextual features ... " (.progress in))]
        (dorun (pmap-chunked 500 extract-context (io/lines-in in)))))))

(defn- sdv [freq_dist]
  (if (zero? (count freq_dist))
    (SparseDoubleVector. (count @CTX_IDS) 0)
    (let [^double sum  (reduce + (vals freq_dist))
          ks   (int-array (sort (keys freq_dist)))
          vs   (double-array (map freq_dist ks))
          card (count @CTX_IDS)
          size (count ks)]
      (SparseDoubleVector. ks vs card size))))



(defn make-sdvs! []
  (reset! COUNTER 0)
  ; filter identity because only observed IVs get atoms during unigram frequency derivation
  ; while confusion sets might include unobserved IVs, and confusion sets are used to make CTX_REL.
  ; might be worth not including DICT in IV to avoid this problem.
  (let [data_atoms (filter identity (concat (map @OOV (.words @OOV)) (map @IV @CTX_REL)))
        handle (fn [data_atom]
                 (swap! data_atom update-in [:context] sdv)
                 (swap! COUNTER inc))]
    (progress/monitor [#(str "Creating SparseDoubleVectors ... " @COUNTER " words processed")]
      (dorun (pmap-chunked 100 handle data_atoms)))))


(defn- measure-constructor [type]
  (fn []
    (eval (read-string (str "(uk.ac.susx.mlcl.byblo.measures."type".)")))))

(defn- mi-measure-constructor [type]
  (fn []
    (let [measure ((measure-constructor type))
          feature_frequencies (double-array (map deref @CTX_FREQS))]
      (println "measure:" measure)
      (doto measure
        (.setFeatureFrequencies feature_frequencies)
        (.setFeatureFrequencySum (reduce + feature_frequencies))))))




(def get-measure-constructor {
  "confusion"    (mi-measure-constructor    "Confusion")
  "cosine"       (measure-constructor       "Cosine")
  "cosinemi"     (mi-measure-constructor    "CosineMi")
  "crmi"         (mi-measure-constructor    "CrMi")
  "dice"         (measure-constructor       "Dice")
  "dicemi"       (mi-measure-constructor    "DiceMi")
  "hindle"       (mi-measure-constructor    "Hindle")
  "jaccard"      (measure-constructor       "Jaccard")
  "jaccardmi"    (mi-measure-constructor    "JaccardMi")
  "jensen"       (measure-constructor       "Jensen")
  "lee"          (measure-constructor       "Lee")
  "lin"          (mi-measure-constructor    "Lin")
  "lp"           (measure-constructor       "Lp")
  "overlap"      (measure-constructor       "Overlap")
  "recallmi"     (mi-measure-constructor    "RecallMi")
  "tanimoto"     (measure-constructor       "Tanimoto")
  })

(def ^:dynamic MEASURE)



(defn get-pair [[word [_ data_atom]]]
  (let [{cs :confusion-set oov_ctx :context} @data_atom]
    (let [left_result (.left MEASURE oov_ctx)

          candidates (loop [acc [] [iv & others] cs]
                       (if iv
                         (if-let [iv_ctx (and (@IV iv) (:context @(@IV iv)))]
                           (recur
                             (conj acc [iv
                                        (.combine MEASURE
                                          (.shared MEASURE oov_ctx iv_ctx)
                                          left_result
                                          (.right MEASURE iv_ctx))])
                             others)
                           (recur acc others))
                         acc))]
      (swap! COUNTER inc)
      (into [word] (last (sort-by second candidates))))))

(def PAIRS (atom []))

(defn reduce-to-pairs! []
  (reset! COUNTER 0)
  (progress/monitor [#(str "Deriving contextually similar pairs ... " @COUNTER " words processed")]
    (swap! PAIRS into (pmap-chunked 100 get-pair @OOV))))


(defn train []
  (setup-tries!)
  (generate-confusion-sets!)
  (await CTX_REL)
  (extract-all-context!)
  (make-sdvs!)
  (binding [MEASURE ((get-measure-constructor (config/opt :train :nmd :measure)))]
    (print "measure:" MEASURE)
    (reduce-to-pairs!))
  ; now done with IV and OOV so reset them and allow garbage collector to do its magic
  (reset! IV nil)
  (reset! OOV nil)

)