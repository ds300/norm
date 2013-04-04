(ns norm.train.twt-c
  (:require [norm.io :as io]
            [norm.data :as data]
            [norm.utils :as utils]
            [norm.words :as words]
            [norm.progress :as progress]))

(defn clean-token? [dict tkn]
  (or (.contains dict tkn) (re-find #"^\p{Punct}+$" tkn)))

(defn is-clean? [dict line]
  (every? (partial clean-token? dict)
    (words/tokenise line)))

(defn ensure-clean [dict line]
  (let [line (.toLowerCase line)]
    (when (is-clean? dict line)
      line)))

(defn train! []
  (data/verify-readable! :dict)

  (data/load-and-bind [:dict]
    (io/open [:r in (data/get-path :twt)
              :w out io/OUT_PATH]
      (progress/monitor [#(str "Filtering tweets ... " (.progress in) "%")]
        (doseq [line (->> (line-seq in)
                       (utils/pmapall-chunked 10000 (partial ensure-clean data/DICT))
                       (filter identity))]
          (.write out (str (words/remove-punct-repetition line) "\n")))))))
