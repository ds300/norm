(ns norm.data
(:require [norm.io :as io]))

(def PATHS (atom {}))

(defn set-paths! [ids dir]
  (doseq [id ids]
    (set-path! id (str dir "./" id))))

(defn set-path! [id path]
  (swap! paths conj [id path]))

(def ^:dynamic NMD)
(def ^:dynamic DICT)
(def ^:dynamic DM-DICT)
(def ^:dynamic TWT)
(def ^:dynamic TWT-C)
(def ^:dynamic TLM)
(def ^:dynamic LKSM)
(def ^:dynamic NYT)
(def ^:dynamic NMD-G)
(def ^:dynamic DPB)

(defn load [id]
  (let [path (PATHS id)])
  (io/doing-done (str "Loading " id " from " path)
    (case id
      "nmd" (into {} (io/parse-tsv path))
      "dict" (trie/trie (map #(conj % 1) (io/parse-tsv path)))
      "nmd-g" (into {} (io/parse-tsv path)))))

(defmacro load-and-bind [ids & body]
  `(clojure.core/binding ~(vec (mapcat (fn [id] [(symbol (.toUpperCase id)) `(norm.core/load ~id)]) ids) )
     (do ~@body)))
