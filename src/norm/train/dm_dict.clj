(ns norm.train.dm-dict
  (require [norm.data :as data]
           [norm.words :as words]
           [norm.io :as io]))

(defn put-word-in-dm-dict [dm-dict word]
  (let [dm_encoding (words/double-metaphone word)]
    (update-in dm-dict [dm_encoding] conj word))) ;remember (conj nil thing) -> '(thing)

(defn train! []
  (data/verify-readable :dict)
  (data/verify-writeable :dm-dict)

  (data/load-and-bind [:dict]
    (io/open [:w out io/OUT_PATH]
      (io/spit-tsv out
        (map
          flatten
          (reduce
            put-word-in-dm-dict
            {}
            (.words data/DICT)))))))