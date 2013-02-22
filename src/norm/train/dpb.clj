(ns norm.train.dpb
  (:require [clojure.data.xml :as xml]
            [norm.config :as config]
            [norm.data :as data]
            [norm.io :as io]
            [norm.progress :as progress]
            [norm.utils :as utils]))


;;;; parsing xml ;;;;

(defn get-kids-by-tag [tag elem]
  (filter #(= tag (:tag %)) (:content elem)))

(defn documents [in]
  (get-kids-by-tag :DOC (xml/parse in)))

(defn sentences [document]
  (mapcat (partial get-kids-by-tag :sentence) (get-kids-by-tag :sentences document)))

(defn tokens [sentence]
  (mapv clojure.string/lower-case
    (for [telem (mapcat (partial get-kids-by-tag :token) (get-kids-by-tag :tokens sentence))]
      (first (:content (first (:content telem)))))))

(defn dependencies [sentence]
  (mapcat :content (get-kids-by-tag :basic-dependencies sentence)))

(defn extract-untyped-deps! [^norm.jvm.Trie DICT iv-ids sentence-counter* dep-counter* sentence]
  (sentence-counter* 1)
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
                        (when (every? #(.contains DICT ^String %) [w1 w2])
                          ; replace words with unique IDs
                          [(iv-ids w1) (iv-ids w2) offset]))))))
        transformed_deps (filter identity (map getd deps))]
    (dep-counter* (count transformed_deps))
    transformed_deps))

(defn filename-filter [^java.io.File file]
  (.. file getName (startsWith "nyt")))

(defn get-absolute-path [^java.io.File file]
  (.getAbsolutePath file))

(defn train! []
  (data/verify-readable! :dict :nyt)

  (data/load-and-bind [:dict]
    (let [sentence-counter* (utils/counter)
          dep-counter*      (utils/counter)
          iv-ids            (utils/unique-id-getter)
          n                 (config/opt :train :dpb :num-sents)

          files             (->> (data/get-path :nyt)
                              (java.io.File.)
                              (.listFiles)
                              (filter filename-filter)
                              (map get-absolute-path)
                              (map io/prog-reader-gz))

          extract-deps!_    (partial extract-untyped-deps! data/DICT iv-ids sentence-counter* dep-counter*)
          
          freqs             (do 
                              (println "Extracting dependencies from up to" n "sentences in nyt corpus...")
                              (progress/monitor [#(str "\t" (sentence-counter*) " sentences processed") 1000]
                                (->> files
                                  (mapcat documents)
                                  (mapcat sentences)
                                  (take n)
                                  (utils/pmapcat extract-deps!_)
                                  (frequencies))))
          
          num_deps          (dep-counter*)] ;deref this once to save processings
        
      (doseq [f files] (.close f))

      (io/open [:w out io/OUT_PATH
                :w out-ids (str io/OUT_PATH "-ids")]
        (io/doing-done "writing to disk"
          (io/spit-tsv out-ids (seq (iv-ids)))
          (io/spit-tsv out (for [[k v] freqs]
                             ; flatten vector and get prob in stead of freq
                             (conj k (/ (double v) num_deps)))))))))
