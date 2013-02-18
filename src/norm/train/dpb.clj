(ns norm.train.dpb
  (:require [clojure.data.xml :as xml]
            [norm.config :as config]
            [norm.data :as data]
            [norm.io :as io]
            [norm.progress :as progress]))



(defn pmapcat [f coll]
  (apply concat
    (pmap f coll)))

(def ^:dynamic *sentence-counter*) ;atom 0
(def ^:dynamic *dep-counter*) ;atom 0

;;;; unique ids for iv words ;;;;

(def IV_IDS (atom {}))

(defn get-id [word]
  (or
    (@IV_IDS word)
    (dec (count (swap! IV_IDS #(assoc % word (count %)))))))

;;;; parsing xml ;;;;

(defn- get-kids-by-tag [tag elem]
  (filter #(= tag (:tag %)) (:content elem)))

(defn- documents [in]
  (get-kids-by-tag :DOC (xml/parse in)))

(defn- sentences [document]
  (mapcat (partial get-kids-by-tag :sentence) (get-kids-by-tag :sentences document)))

(defn- tokens [sentence]
  (mapv clojure.string/lower-case
    (for [telem (mapcat (partial get-kids-by-tag :token) (get-kids-by-tag :tokens sentence))]
      (first (:content (first (:content telem)))))))

(defn- dependencies [sentence]
  (mapcat :content (get-kids-by-tag :basic-dependencies sentence)))

(defn- extract-untyped-deps [sentence]
  (swap! *sentence-counter* inc)
  (let [tkns  (tokens sentence)
        deps  (dependencies sentence)
        getd  (fn [{[{[a] :content} {[b] :content}] :content {type :type} :attrs}]
                ; root dependencies involve a point, and a point is not a word,
                ; so ignore root dependencies
                (when (not= type "root") 
                  (let [i (Integer. a) j (Integer. b) offset (- j i) ]
                    (when (and 
                             ; ensure, just in case, that i and j are >= 0
                            (every? pos? [i j])
                            ; also we only care about tokens up to three
                            ; places removed from the target word
                            (<= (Math/abs offset) 3))
                      ; get the words and check they're in the dictionary
                      (let [w1 (tkns (dec i)) w2 (tkns (dec j))]
                        (when (every? #(.contains data/DICT %) [w1 w2])
                          ; replace words with unique IDs
                          [(get-id w1) (get-id w2) offset]))))))
        transformed_deps (filter identity (map getd deps))]
    (swap! *dep-counter* (partial + (count transformed_deps)))
    transformed_deps))


(defn open [filename]
  (norm.jvm.ProgressTrackingBufferedFileReader/makeGzip filename))


(defn train []
  (binding [*sentence-counter* (atom 0)
            *dep-counter* (atom 0)]
    (let [filenames (map #(.getAbsolutePath %)
                      (filter #(.. % getName (startsWith "nyt"))
                        (.listFiles (java.io.File. (data/get-path :nyt)))))
          n (config/opt :train :dpb :num-sents)]
      (println "Extracting dependencies from up to" n "sentences in nyt corpus...")
      (let [freqs (progress/monitor [#(str "\t" @*sentence-counter* " sentences processed") 1000]
                    (->> filenames
                      (map open)
                      (mapcat documents)
                      (mapcat sentences)
                      (take n)
                      (pmapcat extract-untyped-deps)
                      (frequencies)))
            num_deps @*dep-counter*] ;deref this once to save processings
        (with-open [out (clojure.java.io/writer io/OUT_PATH)
                    out-ids (clojure.java.io/writer (str io/OUT_PATH "-ids"))]
          (io/doing-done "writing to disk"
              (io/spit-tsv out-ids @IV_IDS)
              (io/spit-tsv out (for [[k v] freqs]
                                 ; flatten vector and get prob in stead of freq
                                 (conj k (/ (double v) num_deps))))))))))
