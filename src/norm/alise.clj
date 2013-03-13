(ns norm.alise
  (:require [norm.config :as config]
            [norm.data :as data]
            [norm.utils :as utils]
            [norm.words :as words])
  (:gen-class
    :name "norm.Norm"
    :methods [^{:static true}[getComplexNormaliser [] norm.jvm.Normaliser]
              ^{:static true}[getSimpleNormaliser [] norm.jvm.Normaliser]
              ^{:static true}[getDuplexNormaliser [] norm.jvm.Normaliser]])
  )

(defn at-least [n pred [x & xs :as coll]]
  (cond
    (<= n 0)         true
    (not (seq coll)) false
    (pred x)         (recur (dec n) pred xs)
    :otherwise       (recur n pred xs)))

(defn form-exemplars [dict candidate tkns i]
  (filter identity
    (for [[off gov] (words/indexed-context tkns 3 i)]
      (when (.contains dict gov)
        [gov candidate off]))))

(defn most-frequent [coll]
  (case (count coll)
    0 nil
    1 (first coll)
    (let [[[c1 f1] [c2 f2] & others] (sort-by (comp - second) (frequencies coll))]
      (prn [c1 f1] [c2 f2])
      (.flush *out*)
      (when (or (not f2) (> f1 f2)) c1))))

(defn predict [lksm exemplars]
  (or (most-frequent (map lksm exemplars)) "neg"))

(defn ill-formed? [dict lksm td cs tkns i]
  (let [exemplars_list (for [w cs]
                         (form-exemplars dict w tkns i))]
    (at-least td #(= "pos" %) (map #(predict lksm %) exemplars_list))))

(defn rank-by [f cs original]
  (->> cs
    (map (fn [candidate] [(f candidate original) candidate]))
    sort
    (map last)
    (map vector (range))))

(defn higher-is-better [f]
  (comp - f))

(defn choose-candidate [cs tkns i]
  ;; use only word similarity
  (let [csmap (atom (zipmap cs (repeat 0)))
        update-rank (fn [f]
                      (doseq [[rank candidate] (rank-by f cs (tkns i))]
                        (swap! csmap update-in [candidate] #(+ % rank))))]
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

(defn simple-normalise [nmd tkns]
  (mapv #(nmd % %) tkns))

(defn normalise-token [dict lksm get-cs td tkns i]
  (let [cs (get-cs tkns i)]
    (if (ill-formed? dict lksm td cs tkns i)
      (choose-candidate cs tkns i)
      (tkns i))))

(defn complex-normalise [dict lksm get-cs td tkns]
  (vec
    (for [[i word] (map vector (range) tkns)]
      (if (or (.contains dict word) (not (re-find #"\w[\w\-\d]*")))
        word
        (normalise-token dict lksm get-cs td tkns i)))))

(defn get-complex-normaliser-fn []
  (data/load-and-bind [:nmd :dict :lksm :tlm :dm-dict]
    (let [lex-dist       (config/opt :confusion-sets :lex-dist)
          phon-dist      (config/opt :confusion-sets :phon-dist)
          percent-cutoff (config/opt :confusion-sets :post-rank-cutoff)
          td             1
          get-raw-cs  (partial words/raw-confusion-set data/DICT data/DM-DICT lex-dist phon-dist)
          tlm         data/TLM ; because we use it in a closure, if we use data/TLM directly it will
                               ; be unbound by the time it is used.
          get-cs      (fn [tkns i]
                        (utils/take-percent percent-cutoff
                          (words/lm-ranked-confusion-set tlm get-raw-cs tkns i)))]
      (partial complex-normalise data/DICT data/LKSM get-cs td))))

(defn get-simple-normaliser-fn []
  (partial simple-normalise (data/load- :nmd)))

(defn ^{:static true} -getComplexNormaliser []
  (norm.jvm.Normaliser. (get-complex-normaliser-fn)))

(defn ^{:static true} -getSimpleNormaliser []
  (norm.jvm.Normaliser. (get-simple-normaliser-fn)))

(defn ^{:static true} -getDuplexNormaliser []
  (norm.jvm.Normaliser. (comp (get-complex-normaliser-fn) (get-simple-normaliser-fn))))
