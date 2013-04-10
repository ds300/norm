(ns norm.train.dm-dict
  (require [norm.data :as data]
           [norm.words :as words]
           [norm.io :as io]))

(defn train! []
  (data/verify-readable! :dict)

  (data/load-and-bind [:dict]
    (io/open [:w out io/OUT_PATH]
      (io/spit-tsv out
        (map
          flatten
          (group-by
            words/double-metaphone
            (.words data/DICT)))))))