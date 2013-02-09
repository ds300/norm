(ns norm.train.twt-c
  (:require [norm.io :as io]
            [norm.data :as data]
            [norm.trie :as trie]
            [norm.words :as words]
            [norm.progress :as progress]))

(defn is-clean [line]
  (every?
    #(or (re-find #"^\p{Punct}+$" %) (trie/contains data/DICT (.toLowerCase %)))
    (words/tokenise line)))

(defn filter-in-parallel [lines]
  (apply concat
    (pmap
      (fn [chunk] (filter 
                    #(and (not-empty %) (is-clean %))
                    chunk))
      (partition-all 1000 lines))))

(defn train []
  (let [in (io/prog-reader (data/get-path :twt))]
    (progress/monitor [#(str "Filtering tweets ... " (.progress in) "%")]
      (doseq [line (filter-in-parallel (io/lines-in in))]
        (.write io/OUT (str (words/remove-punct-repetition line) "\n"))))))
