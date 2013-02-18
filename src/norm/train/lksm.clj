(ns norm.train.lksm
  (:import [de.bwaldvogel.liblinear Linear Problem Parameter SolverType]
            [java.io File])
  (:require [norm.config :as config]
            [norm.io :as io]
            [norm.data :as data]
            [norm.progress :as progress]
            [norm.words :as words]
            [norm.utils :as utils]))

(defn extract-positive-features [DICT tokens i]
  (let [dependent (tokens i)
        lctx (map vector (iterate dec -1) (reverse (words/context-left tokens 3 i)))
        rctx (map vector (iterate inc 1) (words/context-right tokens 3 i))]
    (for [[offset governor] (filter #(.contains DICT (last %)) (concat lctx rctx))]
      [true governor dependent offset])))

(defn derive-negative-features [positive_features confusion_set]
  (for [[_ gov dep off] positive_features word confusion_set]
    [false gov word off]))

(defn extract-features!
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

(defn encode-feature-vector [dpb-score-feature-id [posneg gov_id dep_id off_id score]]
  (apply str 
    (if posneg 1 0) " " 
    dpb-score-feature-id ":" score " "
    (apply str (flatten (interpose " " (sort [[gov_id ":1"] [dep_id ":1"] [off_id ":1"]]))))
    "\n"))

(defn store-features:first-pass! [feature-predicate pos-feature-vector-ids tmpout feats]
  (doseq [[posneg gov_id dep_id off_id score :as f] (filter feature-predicate feats)]
    (when posneg (pos-feature-vector-ids [gov_id dep_id off_id])) ; keep track of positive feature vector ids
    (.write tmpout (str (pr-str f) "\n"))))

(defn store-features:second-pass! [to-svm-format feature-predicate tmpin tmpout]
  (doseq [line  (->> tmpin
                  line-seq
                  (filter not-empty)
                  (map read-string)
                  (filter feature-predicate)
                  (map to-svm-format))]
    (.write tmpout line)))


(defn confusion-set-getter [dict dm-dict tlm lex-dist phon-dist n]
  (let [get-cs (memoize (partial words/raw-confusion-set dict dm-dict lex-dist phon-dist))]
    (fn [tokens i]
      (take n (words/lm-ranked-confusion-set tlm get-cs tokens i)))))


(defn train []
  (let [tmp1_path (str io/OUT_PATH ".tmp1")
        tmp2_path (str io/OUT_PATH ".tmp2")
        bias      (config/opt :train :lksm :bias)
        eps       (config/opt :train :lksm :eps)
        c         (config/opt :train :lksm :c)
        solver    (eval (symbol (str "de.bwaldvogel.liblinear.SolverType/" (.toUpperCase (config/opt :train :lksm :solver)))))]
    (data/load-and-bind [:dict :dm-dict :tlm :dpb]
      (let [feature-ids             (utils/unique-id-getter)
            iv-ids                  (utils/unique-id-getter)
            pos-feature-vector-ids  (utils/unique-id-getter)
            lex_dist                (config/opt :confusion-sets :lex-dist)
            phon_dist               (config/opt :confusion-sets :phon-dist)
            num_candidates          (config/opt :train :lksm :num-candidates)
            get-confusion-set       (confusion-set-getter data/DICT data/DM-DICT data/TLM lex_dist phon_dist num_candidates)]
        (feature-ids :garbage) ; do this because liblinear doesn't like indices to start at 0
        (feature-ids :dpb-score) ;add this in because it won't get done automatically
        (utils/let-partial [(extract-features! data/DICT data/DPB feature-ids iv-ids get-confusion-set)
                            (legit-feature? pos-feature-vector-ids)
                            (encode-feature-vector (feature-ids :dpb-score))
                            (store-features:first-pass! legit-feature? pos-feature-vector-ids)
                            (store-features:second-pass! encode-feature-vector legit-feature?)]
          (println "Extracting feature-vectors: first pass")
          (with-open [in (io/prog-reader (data/get-path :twt-c))
                      out (clojure.java.io/writer tmp1_path)]
            (progress/monitor [#(str "\t" (.progress in)) 1000]
              (->> in
                (line-seq)
                (filter not-empty)
                (map (comp words/tokenise clojure.string/lower-case))
                (utils/pmapcat extract-features!)
                (store-features:first-pass! out))
              (.flush out))
          (println "Extracting feature-vectors: second pass")
          (with-open [in (io/prog-reader tmp1_path)
                      out (clojure.java.io/writer tmp2_path)]
            (progress/monitor [#(str "\t" (.progress in)) 1000]
              (store-features:second-pass! in out)
              (.flush out)))))))

    ; delete tmp1 and collect garbage before proceeding with model training.
    (.delete (File. tmp1_path))
    (dotimes [_ 5] (System/gc))

    (let [problem   (io/doing-done "Loading Problem" (Problem/readFromFile (File. tmp2_path) bias))
          parameter (Parameter. solver c eps)
          model     (io/doing-done "Training model" (Linear/train problem parameter))]
      (.delete (File. tmp2_path))
      (io/doing-done "Writing model to disk"
        (.save model (File. io/OUT_PATH))))))

