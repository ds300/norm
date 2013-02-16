(ns norm.train.lksm
  (:require [norm.io :as io]
            [norm.data :as data]
            [norm.words :as words]
            [norm.utils :as utils]))

(def ^:private feature-ids (utils/unique-id-getter))

(feature-ids :dpb-score) ; ensure this is 0 so we can hard-code it when storing features.

(def ^:private pos-feature-vector-ids (utils/unique-id-getter))

(def ^:private iv-ids (utils/unique-id-getter))


(defn extract-positive-features [DICT tokens i]
  (let [dependent (tokens i)
        lctx (for [[i v] (utils/indexify 1 (reverse (words/context-left tokens 3 i)))] [(- i) v])
        rctx (utils/indexify 1 (words/context-right tokens 3 i))]
    (for [[offset governor] (filter #(.contains DICT %) (concat lctx rctx))]
      [true governor dependent offset])))

(defn derive-negative-features [positive_features confusion_set]
  (for [[_ gov dep off] positive_features word confusion_set]
    [false gov word off]))

(defn extract-features
  "get-cs = (fn [tokens index] confusion_set)"
  [DICT DPB feature-ids iv-ids get-confusion-set tokens]
  (filter identity
    (apply concat
      (for [i (range (count tokens))]
        (when (.contains DICT (tokens i))
          (let [confusion_set (get-confusion-set tokens i)
                pos (extract-positive-features DICT tokens i)
                neg (derive-negative-features pos confusion_set)]
            ;; get feature-ids of iv-ids to save space
            ;; also get DPB scores
            (for [[posneg gov dep off] (concat pos neg)]
              [  
                posneg
                (feature-ids [0 (iv-ids gov)])
                (feature-ids [1 (iv-ids dep)])
                (feature-ids off)
                (DPB gov dep off)
              ]
            )))))))

(defn legit-feature?
  "illegitimate features are negative ones which have already been seen as positive ones"
  [pos-feature-vector-ids [posneg gov_id dep_id off_id score]]
  (or
    posneg
    (pos-feature-vector-ids [gov_id dep_id off_id] false)))

(defn store-features:first-pass [legit-feature? pos-feature-vector-ids feats tmpout]
  (binding [*out* tmpout]
    (doseq [[posneg gov_id dep_id off_id score :as f] (filter legit-feature? feats)]
      (when posneg (pos-feature-vector-ids [gov_id dep_id off_id])) ; keep track of positive feature vector ids
      (prn f))))


(defn store-features:second-pass [legit-feature? tmpin tmpout]
  (doseq [[posneg gov_id dep_id off_id score] (->> tmpin
                                                line-seq
                                                (filter not-empty)
                                                (map read-string)
                                                (filter legit-feature?))]
    (.write tmpout 
      (apply str 
        (if posneg 1 0) " " 
        "0:" score " "
        (flatten (interpose " " (sort [[gov_id ":1"] [dep_id ":1"] [off_id ":1"]])))
        "\n"))))

(defn load-problem [input_path bias]
  (de.bwaldvogel.liblinear.Problem/loadFromFile (java.io.File. input_path) bias))

(defn get-confusion-set [dict dm-dict tlm lex-dist phon-dist n tokens i]
  (let [get-cs (partial words/raw-confusion-set dict dm-dict lex-dist phon-dist)]
    (take n (words/lm-ranked-confusion-set tlm get-cs tokens i))))


(defn train []
  (data/load-and-bind [:dict :dm-dict :tlm :dpb]
    (let [feature-ids    (utils/unique-id-getter)
          iv-ids         (utils/unique-id-getter)
          lex_dist       (config/opt :confusion-sets :lex-dist)
          phon_dist      (config/opt :confusion-sets :phon_dist)
          num_candidates (config/opt :train :lksm :num-candidates)
          bias           (config/opt :train :lksm :bias)
          tmp1_path      (str io/OUT_PATH ".tmp1")
          tmp2_path      (str io/OUT_PATH ".tmp2")]
      (feature-ids :dpb-score)
      (utils/let-partial [(get-confusion-set data/DICT data/DM-DICT data/TLM lex_dist phon_dist num_candidates)
                          (extract-features data/DICT data/DPB feature-ids iv-ids get-confusion-set)]))
    
    (println "Extracting feature vectors: first pass")))

