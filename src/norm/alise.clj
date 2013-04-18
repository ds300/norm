(ns norm.alise
  "This is where all the normalisation bits happen."
  (:require [norm.config :as config]
            [norm.data :as data]
            [norm.utils :as utils]
            [norm.words :as words])
  (:gen-class
    :name "norm.Norm"
    :methods [^{:static true}[getComplexNormaliser [] norm.jvm.Normaliser]
              ^{:static true}[getSimpleNormaliser [] norm.jvm.Normaliser]
              ^{:static true}[getDuplexNormaliser [] norm.jvm.Normaliser]]))

(defn form-exemplars
  "makes a bunch of candidate exemplars"
  [dict candidate tkns i]
  (filter identity
    (for [[off gov] (words/indexed-context tkns 3 i)]
      (when (.contains dict gov)
        [gov candidate off]))))

(defn most-frequent
  "returns the most frequent element in coll, when it is definitely
  the most frequent element. If it is a tie, nil is returned."
  [coll]
  (case (count coll)
    0 nil
    1 (first coll)
    (let [[[c1 f1] [c2 f2] & others] (sort-by (comp - second) (frequencies coll))]
      (when (or (not f2) (> f1 f2)) c1))))

(defn predict
  "Takes some exemplars and gets the most frequent prediction from lksm"
  [lksm exemplars]
  (or (most-frequent (map lksm exemplars)) "neg"))

(defn ill-formed?
  "decides whether (tkns i) is ill-formed or not, in the context of
  its confusion set cs, using lksm."
  [dict lksm td cs tkns i]
  (let [exemplars_list (for [w cs]
                         (form-exemplars dict w tkns i))]
    (utils/at-least td #(= "pos" %) (map #(predict lksm %) exemplars_list))))

(defn rank-by
  "ranks cs by (partial f original)"
  [f cs original]
  (->> cs
    (group-by (partial f original))
    sort
    (map last)
    (map vector (range))))

(defn higher-is-better
  "returns -f. For readability"
  [f]
  (comp - f))

(defn choose-candidate
  "given the original word and its confusion set, chooses a
  candidate to replace it."
  [cs orig]
  ;; use only word similarity
  (let [csmap (atom (zipmap cs (repeat 0)))
        update-rank (fn [f]
                      (doseq [[rank candidates] (rank-by f cs orig)]
                        (doseq [candidate candidates]
                          (swap! csmap update-in [candidate] #(+ % rank)))))]
    (dorun
      (map update-rank
        [words/levenshtein
         (higher-is-better words/ssk)
         (higher-is-better words/longest-common-subsequence)
         (higher-is-better words/common-prefix-length)
         (higher-is-better words/common-suffix-length)]))
    (->> (sort-by last (seq @csmap))
      first
      first)))

(defn simple-normalise
  "normalises a list of tokens using the simple strategy"
  [dict nmd tkns]
  (mapv #(if (.contains dict %) % (nmd % %)) tkns))

(defn normalise-token
  "complex-normalises a the token in tkns at index i"
  [dict lksm get-cs td tkns i]
  (let [cs (get-cs tkns i)]
    (if (ill-formed? dict lksm td cs tkns i)
      (choose-candidate cs (nth tkns i))
      (nth tkns i))))

(defn complex-normalise
  "normalises a list of tokens using the complex strategy"
  [dict lksm get-cs td tkns]
  (let [tkns (vec tkns)]
    (vec
      (for [[i word] (map vector (range) tkns)]
        (if (or (.contains dict word) (not (re-find #"^\w[\w\-\d']*$" word)))
          word
          (normalise-token dict lksm get-cs td tkns i))))))

(defn get-cs-getter
  "returns a fn that gets a confusion set when given tkns and i"
  [dict dm-dict tlm lex-dist phon-dist percent-cutoff]
  (let [get-raw-cs (partial words/raw-confusion-set dict dm-dict lex-dist phon-dist)]
    (fn [tkns i]
      (utils/take-percent percent-cutoff
        (words/lm-ranked-confusion-set tlm get-raw-cs tkns i)))))


(defn get-complex-normaliser-fn
  "returns a function which normalises token lists using the complex strategy"
  []
  (data/load-and-bind [:dict :lksm :tlm :dm-dict]
    (partial complex-normalise
      data/DICT
      data/LKSM
      (get-cs-getter
        data/DICT
        data/DM-DICT
        data/TLM
        (config/opt :confusion-sets :lex-dist)
        (config/opt :confusion-sets :phon-dist)
        (config/opt :confusion-sets :post-rank-cutoff))
      1)))

(defn get-simple-normaliser-fn
  "returns a function which normalises token lists using the simple strategy"
  []
  (partial simple-normalise (data/load- :dict) (data/load- :nmd)))

(defn get-duplex-normaliser-fn
  "returns a function which normalises token lists using the duplex strategy"
  []
  (comp (get-complex-normaliser-fn) (get-simple-normaliser-fn)))

(defn ^{:static true} -getComplexNormaliser []
  (norm.jvm.Normaliser. (get-complex-normaliser-fn)))

(defn ^{:static true} -getSimpleNormaliser []
  (norm.jvm.Normaliser. (get-simple-normaliser-fn)))

(defn ^{:static true} -getDuplexNormaliser []
  (norm.jvm.Normaliser. (get-duplex-normaliser-fn)))
