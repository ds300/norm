(ns norm.train.lksm
  (:require [norm.io :as io]
            [norm.data :as data]
            [norm.words :as words]))

(def ^:dynamic *iv-ids* {})

(defn- trigrams [lines]
  (mapcat (partial words/n-grams 3)
    (map (comp words/tokenise clojure.string/lower-case) lines)))