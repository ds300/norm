(ns norm.train.dpb
  (:require [clojure.data.xml :as xml]
            [norm.data :as data]
            [norm.io :as io]
            [norm.progress :as progress]))

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
  (let [tkns (tokens sentence)
        deps (dependencies sentence)
        get  (fn [{[{[a] :content} {[b] :content}] :content {type :type} :attrs}]
               (when (not= type "root")
                 (let [i (Integer. a) j (Integer. b) offset (- j i) ]
                   (when (and 
                           (every? pos? [i j])
                           (<= (Math/abs offset) 3))
                     (let [w1 (tkns (dec i)) w2 (tkns (dec j))]
                       (when (every? #(.contains data/DICT %) [w1 w2])
                         [w1 w2 offset]))))))]
    (filter identity (map get deps))))

(defn- file-freqs [filename]
  (println "Extracting dependencies from" filename "...")
  (with-open [in (norm.jvm.ProgressTrackingBufferedFileReader/makeGzip filename)]
    (progress/monitor [#(str "\t" (.progress in))]
      (frequencies
        (mapcat extract-untyped-deps
          (mapcat sentences
            (documents in)))))))



(defn train []
  (let [filenames (map #(.getAbsolutePath %) (.listFiles (java.io.File. (data/get-path :nyt))))
        freqs (apply (partial merge-with +) (doall (map file-freqs filenames)))]
    (io/doing-done "writing to disk" (io/spit-tsv io/OUT (map flatten freqs)))))
