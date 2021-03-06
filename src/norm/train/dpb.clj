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

(defn extract-untyped-deps! [^norm.jvm.Trie DICT sentence-counter* dep-counter* store-fn* sentence]
  (sentence-counter* 1)
  (let [tkns (tokens sentence)]
    (doseq [{[{[a] :content} {[b] :content}] :content {type :type} :attrs} (dependencies sentence)]
      (when (not= type "root") 
        (try
          (let [i (Integer. (.trim a)) j (Integer. (.trim b)) offset (- i j) ]
            (when (and 
                     ; ensure, just in case, that i and j are >= 0
                    (every? pos? [i j])
                    ; also we only care about tokens up to three
                    ; places removed from the target word
                    (<= (Math/abs offset) 3))
              ; get the words and check they're in the dictionary
              (let [w1 (tkns (dec i)) w2 (tkns (dec j))]
                (when (and 
                        (.contains DICT ^String w1)
                        (not (.contains DICT ^String w2)))
                  ; replace words with unique IDs
                  (store-fn* [w1 offset])
                  (dep-counter* 1)))))
          (catch NumberFormatException e (do nil)))))))

(defn filename-filter [^java.io.File file]
  (.. file getName (startsWith "nyt")))

(defn get-absolute-path [^java.io.File file]
  (.getAbsolutePath file))

(defn store! [iv_freq_maps* [w off]]
  (swap! (iv_freq_maps* w) update-in [off] (fnil inc 0)))

(defn handle-file! [sentence-handler! f]
  (doseq [document (documents f)]
    (doseq [sentence (sentences document)]
      (sentence-handler! sentence))))

(defn dp [v n]
  (let [x (Math/pow 10 n)]
    (/ (int (* v x)) x)))

(defn used-memory []
  (dp
    (/
      (double
        (- (.totalMemory (Runtime/getRuntime))
           (.freeMemory (Runtime/getRuntime))))
      (* 1024 1024 1024))
    3))

(defn train! []
  (data/verify-readable! :dict :nyt)

  (data/load-and-bind [:dict]
    (let [sentence-counter* (utils/counter)
          dep-counter*      (utils/counter)
          iv_freq_maps*     (into {} (map vector (.words data/DICT) (repeatedly #(atom {}))))
          store!_           (partial store! iv_freq_maps*)
          n                 (config/opt :train :dpb :num-sents)

          files             (->> (data/get-path :nyt)
                              (java.io.File.)
                              (.listFiles)
                              (filter filename-filter)
                              (map get-absolute-path)
                              (map io/reader-gz))

          extract-deps!_    (partial extract-untyped-deps! data/DICT sentence-counter* dep-counter* store!_)
          handle-file!_     (partial handle-file! extract-deps!_)]
      
      (println "Extracting dependencies from up to" n "sentences in nyt corpus...")
      (progress/monitor [#(str "\t" (sentence-counter*) " sentences processed. Using " (used-memory)) 2000]
        (dorun
          (utils/pmapall handle-file!_ files)))


      (doseq [f files] (.close f))

      (io/open [:w out io/OUT_PATH]
        (io/doing-done "writing to disk"
          (let [num_deps (dep-counter*)]
            (io/spit-tsv out
              (for [[w freq_atom] (filter (comp not-empty deref last) iv_freq_maps*)]
                (flatten [w (for [[k v] @freq_atom]
                              [k (/ (double v) num_deps)])])))))))))
