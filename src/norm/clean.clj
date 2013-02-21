(ns norm.clean
  (:import [com.cybozu.labs.langdetect DetectorFactory LangDetectException])
  (:require [norm.io :as io]
            [norm.words :as words]
            [norm.progress :as progress]
            [norm.utils :as utils]
            [norm.data :as data]))

(set! *warn-on-reflection* true)
; filter english, remove duplicates fuzzily, remove tweets without at least 3 words.

(defn canonicalise
  "Removes information from the line to some extent which might
  be useful in the task of near-duplicate detection."
  [line]
  (mapv #(take 4 %) (words/word-tokenise line)))

(defn get-dupe-filter
  "returns a filter which, rather incoherently, returns false if
  it has probably seen the given text before."
  []
  (let [seen (atom #{})]
    (fn [line]
      (let [canon (canonicalise line)]
        (if (or (< (count canon) 3) (@seen canon))
          false
          (do
            (swap! seen conj canon)
            true))))))

(defn get-english-filter
  "Returns a predicate function which takes some text and returns
  true iff the text appears to be english."
  []
  (DetectorFactory/loadProfile (data/get-path :profiles))
  (fn [line]
    (try 
      (let [detector (DetectorFactory/create)]
        (.append detector ^String line)
        (= "en" (.detect detector)))
      (catch LangDetectException e (do false)))))

(defn get-filters
  "given a collection of filter names as strings, returns the appropriate
  predicate funtions. Throws IllegalArgumentException if unrecognised
  filter given."
  [filters]
  (into [identity]
    (map
      #((or
        ({"english" get-english-filter "duplicates" get-dupe-filter} %)
        (throw (IllegalArgumentException. (str "Bad filter: " %)))))
      filters)))

(defn clean
  "takes tweets from in_path and writes them to out_path if they pass
  through the specified filters. Current filter options are 'english' 
  and 'duplicates'."
  [in_path out_path & filters]
  (let [clean? (apply every-pred (get-filters filters))]
    (io/open [:r in in_path
              :w out out_path]
      (progress/monitor [#(.progress in) 1000]
        (doseq [line (->> in
                       line-seq
                       (partition-all 10000)
                       (pmap (partial filter clean?))
                       (apply concat))]
          (.write out (str line "\n")))))))

