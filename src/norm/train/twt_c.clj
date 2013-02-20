(ns norm.train.twt-c
  (:require [norm.io :as io]
            [norm.data :as data]
            [norm.words :as words]
            [norm.progress :as progress]))

(defn is-clean? [DICT line]
  (every?
    #(or (re-find #"^\p{Punct}+$" %) (.contains DICT %))
    (words/tokenise line)))

(defn filter-in-parallel [DICT lines]
  (apply concat
    (pmap
      (fn [chunk] (filter 
                    #(and (not-empty %) (is-clean? DICT %))
                    chunk))
      (partition-all 1000 lines))))

(defn train! []
  (data/verify-readable! :dict)

  (data/load-and-bind [:dict]
    (io/open [:r in (data/get-path :twt)
              :w out io/OUT_PATH]
      (progress/monitor [#(str "Filtering tweets ... " (.progress in) "%")]
        (doseq [line  (filter-in-parallel data/DICT
                        (map clojure.string/lower-case
                          (line-seq in)))]
          (.write out (str (words/remove-punct-repetition line) "\n")))))))
