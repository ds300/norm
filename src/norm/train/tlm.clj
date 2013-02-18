(ns norm.train.tlm
  (:require [norm.data :as data]
            [norm.io :as io]))

(defn train! []
  (let [tmp_path (str io/OUT_PATH ".tmp")]
    
    (edu.berkeley.nlp.lm.io.MakeKneserNeyArpaFromText/main
      (into-array String ["3" tmp_path (data/get-path :twt-c)]))

    (edu.berkeley.nlp.lm.io.MakeLmBinaryFromArpa/main
      (into-array String ["-h" tmp_path io/OUT_PATH]))

    (.delete (java.io.File. tmp_path))))