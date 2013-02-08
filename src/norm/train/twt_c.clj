(ns norm.train.twt-c
  (:require [norm.io :as io]
            [norm.data :as data]
            [norm.trie :as trie]
            [norm.words :as words]))

(defn is-clean [line]
  (every?
    #(or (re-find #"^\p{Punct}+$" %) (trie/contains data/DICT (.toLowerCase %)))
    (words/tokenise line)))

(defn train []
  (let [in (io/prog-reader (@data/PATHS :twt))]
    (dorun
      (map
        #(.write io/OUT (str % "\n"))
        (filter is-clean (filter not-empty (io/lines-in in)))))))
