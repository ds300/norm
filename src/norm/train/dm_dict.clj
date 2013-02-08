(ns norm.train.dm-dict
  (require [norm.data :as data]
           [norm.words :as words]
           [norm.io :as io]))

(defn train []
  (io/spit-tsv io/OUT
    (map
      (fn [[k vs]] (cons k vs))
      (reduce
        (fn [m w]
          (let [dm (words/double-metaphone w)]
            (update-in m [dm] conj w)))
        {}
        (map first data/DICT)))))