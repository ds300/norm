(ns norm.train.lksm
  (:import [de.bwaldvogel.liblinear Linear Problem Parameter SolverType]
            [java.io File])
  (:require [norm.config :as config]
            [norm.io :as io]
            [norm.data :as data]
            [norm.progress :as progress]
            [norm.words :as words]
            [norm.utils :as utils]))

(defn extract-positive-features
  "returns contextual features for the token at i in tokens.
  Such features take the form [true governor dependent offset]
  Where dependent is the token at i in tokens, and governer is
  the token at i + offset in tokens."
  [DICT tokens i]
  (let [dependent (tokens i)
        context   (words/indexed-context tokens 3 i)]
    (for [[offset governor] (filter #(.contains DICT (last %)) context)]
      [true governor dependent offset])))

(defn derive-negative-features
  "Takes some positive features and a confusion set for a particular
  word, and returns copies of the positive features where the first item
  has been changed from true to false, and the word has been replaced
  by those in its confusion set"
  [positive_features confusion_set]
  (for [[_ gov dep off] positive_features word confusion_set]
    [false gov word off]))

(defn extract-features!
  "Extracts features for the given line, where feature-ids* is
  a function that returns a unique id for features, iv-ids* is a function
  which returns unique ids for iv words, and get-confusion-set is a function
  which takes a token list and an index and returns an lm-ranked confusion set."
  [DICT DPB feature-ids* iv-ids* get-confusion-set tweet-counter* line]
  (tweet-counter* 1)
  (let [tokens (words/tokenise-lower line)]
    (filter identity
      (apply concat
        (for [i (range (count tokens))]
          (when (.contains DICT (tokens i))
            (let [confusion_set (filter #(not= % (nth tokens i)) (get-confusion-set tokens i))
                  pos (extract-positive-features DICT tokens i)
                  neg (derive-negative-features pos confusion_set)]
              ;; get feature-ids* of iv-ids* to save space
              ;; also get DPB scores
              (for [[posneg gov dep off] (concat pos neg)]
                [  
                  posneg
                  (feature-ids* [0 (iv-ids* gov)])
                  (feature-ids* [1 (iv-ids* dep)])
                  (feature-ids* off)
                  (DPB gov off)
                ]
              ))))))))

(defn legit-feature?
  "illegitimate features are negative ones which have already been seen as positive ones"
  [pos-feature-vector-ids* [posneg gov_id dep_id off_id score]]
  (or
    posneg
    (pos-feature-vector-ids* [gov_id dep_id off_id] false)))

(defn encode-feature-vector
  "takes a feature vector in the format used in this program
  and converts it to a string representation of the format used
  by libsvm"
  [dpb-score-feature-id [posneg gov_id dep_id off_id score]]
  (apply str 
    (if posneg 1 0) " " 
    dpb-score-feature-id ":" score " "
    (apply str (flatten (interpose " " (sort [[gov_id ":1"] [dep_id ":1"] [off_id ":1"]]))))
    "\n"))

(defn store-features:first-pass!
  "Takes a seq of features and writes them to tmpout"
  [feature-predicate pos-feature-vector-ids* tmpout feats]
  (doseq [[posneg gov_id dep_id off_id score :as f] (filter feature-predicate feats)]
    (when posneg (pos-feature-vector-ids* [gov_id dep_id off_id])) ; keep track of positive feature vector ids
    (.write tmpout (str (pr-str f) "\n"))))

(defn store-features:second-pass!
  "takes the features produced in the first pass and filters
  out cases where negative feature vectors have already been seen
  as positive ones. Writes them to tmpout in libsvm format."
  [to-svm-format feature-predicate tmpin tmpout]
  (doseq [line  (->> tmpin
                  line-seq
                  (filter not-empty)
                  (map read-string)
                  (filter feature-predicate)
                  (map to-svm-format))]
    (.write tmpout line)))


(defn confusion-set-getter
  "Returns a function which takes a list of tokens and an index,
  and returns an lm-ranked confusion set with the settings specified
  by the arguments."
  [dict dm-dict tlm lex-dist phon-dist n]
  (let [get-cs (memoize (partial words/raw-confusion-set dict dm-dict lex-dist phon-dist))]
    (fn [tokens i]
      (take n (words/lm-ranked-confusion-set tlm get-cs tokens i)))))


(defn train! []
  (data/verify-readable! :dict :dm-dict :tlm :dpb :twt-c)

  (let [tmp1_path (str io/OUT_PATH ".tmp1")
        tmp2_path (str io/OUT_PATH ".tmp2")
        feature_ids_path  (str io/OUT_PATH "-f-ids")
        iv_ids_path       (str io/OUT_PATH "-iv-ids")
        bias      (config/opt :train :lksm :bias)
        eps       (config/opt :train :lksm :eps)
        c         (config/opt :train :lksm :c)
        solver    (eval (symbol (str "de.bwaldvogel.liblinear.SolverType/" (.toUpperCase (config/opt :train :lksm :solver)))))]
    (data/load-and-bind [:dict :dm-dict :tlm :dpb]
      (let [feature-ids*              (utils/unique-id-getter 1)
            pos-feature-vector-ids*   (utils/unique-id-getter)
            iv-ids*                   (utils/unique-id-getter)
            lex_dist                  (config/opt :confusion-sets :lex-dist)
            phon_dist                 (config/opt :confusion-sets :phon-dist)
            num_candidates            (config/opt :train :lksm :num-candidates)
            chunksize                 (config/opt :train :lksm :chunksize)
            num_tweets                (config/opt :train :lksm :num-tweets)
            tweet-counter*            (utils/counter)
            get-confusion-set         (confusion-set-getter data/DICT data/DM-DICT data/TLM lex_dist phon_dist num_candidates)
            extract-feats!_           (partial extract-features! data/DICT data/DPB feature-ids* iv-ids* get-confusion-set tweet-counter*)
            legit-feat?_              (partial legit-feature? pos-feature-vector-ids*)
            encode-feat_              (partial encode-feature-vector (feature-ids* :dpb-score))
            store-feats-1!_           (partial store-features:first-pass! legit-feat?_ pos-feature-vector-ids*)
            store-feats-2!_           (partial store-features:second-pass! encode-feat_ legit-feat?_)]

        (println "Extracting feature-vectors: first pass")
        (io/open [:r in (data/get-path :twt-c)
                  :w out tmp1_path]
          (progress/monitor [#(str "\t" (tweet-counter*) " tweets processed") 6000]
            (->> in
              (line-seq)
              (filter not-empty)
              (take num_tweets)
              (utils/pmapall-chunked chunksize extract-feats!_)
              (apply concat)
              (store-feats-1!_ out)

              )))

        (io/doing-done "Storing feature-ids"
          (io/open [:w out feature_ids_path]
            (io/spit-tsv out (seq (feature-ids*)))))

        (io/doing-done "Storing iv-ids"
          (io/open [:w out iv_ids_path]
            (io/spit-tsv out (seq (iv-ids*)))))
        
        (println "Extracting feature-vectors: second pass")
        (io/open [:r in tmp1_path
                  :w out tmp2_path]
          (progress/monitor [#(str "\t" (.progress in)) 1000]
            (store-feats-2!_ in out)))))

    ; delete tmp1 and collect garbage before proceeding with model training.
    (.delete (File. tmp1_path))
    (dotimes [_ 5] (System/gc))

    (let [problem   (io/doing-done "Loading Problem" (Problem/readFromFile (File. tmp2_path) bias))
          parameter (Parameter. solver c eps)
          model     (io/doing-done "Training model" (Linear/train problem parameter))]
      (.delete (File. tmp2_path))
      (io/doing-done "Writing model to disk"
        (.save model (File. io/OUT_PATH))))))

