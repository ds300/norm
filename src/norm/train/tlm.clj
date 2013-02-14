(ns norm.train.tlm
  (:require [norm.data :as data]
            [norm.io :as io]))


(defn train []

  (edu.berkeley.nlp.lm.io.MakeKneserNeyArpaFromText/main
    (into-array String ["3" (str io/OUT_PATH ".tmp") (data/get-path :twt-c)]))

  (edu.berkeley.nlp.lm.io.MakeLmBinaryFromArpa/main
    (into-array String ["-h" (str io/OUT_PATH ".tmp") io/OUT_PATH]))

  (.delete (java.io.File. (str io/OUT_PATH ".tmp"))))