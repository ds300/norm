(ns norm.clean
  (:import [com.cybozu.labs.langdetect DetectorFactory LangDetectException])
  (:require [norm.io :as io]
            [norm.words :as words]
            [norm.progress :as progress]
            [norm.utils :as utils]
            [norm.data :as data]))

; filter english, remove duplicates fuzzily, remove tweets without at least 3 words.

(defn canonicalise [line]
  (mapv #(take 4 %) (words/word-tokenise line)))

(defn get-dupe-filter []
  (let [seen (atom #{})]
    (fn [line]
      (let [canon (canonicalise line)]
        (if (or (< (count canon) 3) (@seen canon))
          false
          (do
            (swap! seen conj canon)
            true))))))

(defn get-english-filter []
  (DetectorFactory/loadProfile (data/get-path :profiles))
  (fn [line]
    (try 
      (let [detector (DetectorFactory/create)]
        (.append detector ^String line)
        (= "en" (.detect detector)))
      (catch LangDetectException e (do false)))))

(defn get-filters [filters]
  (into []
    (map
      #((or
        ({"english" get-english-filter "duplicates" get-dupe-filter} %)
        (throw (IllegalArgumentException. (str "Bad filter: " %)))))
      filters)))

(defn clean [in_path out_path & filters]
  (let [fs (get-filters filters)
        clean? (apply every-pred fs)]
    (io/open [:r in in_path
              :w out out_path]
      (progress/monitor [#(.progress in) 1000]
        (doseq [line (->> in
                       line-seq
                       (partition-all 50)
                       (pmap (partial filter clean?))
                       (apply concat))]
          (.write out (str line "\n")))))))

