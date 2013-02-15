(ns norm.data
(:require [norm.io :as io]
          [norm.trie :as trie]
          [norm.config :as config]))

(def FILES [
  :nmd
  :dict
  :dm-dict
  :twt
  :twt-c
  :tlm
  :lksm
  :nyt
  :nmd-g
  :dpb
  :ulm
])

(doseq [id (map (comp symbol clojure.string/upper-case name) FILES)]
  (.setDynamic (intern 'norm.data id)))


(defn set-path! [id path]
  (println "Setting " id " to " path)
  (swap! config/OPTS assoc-in [:data :paths id] path))


(defn get-path [id]
  (or
    (get-in @config/OPTS [:data :paths id])
    (str (get-in @config/OPTS [:data :dir]) "/" (name id))))

(defn load-dpb-data [path]
  (let [[ids deps] (partition-by count (io/parse-tsv path))]
    [
      (into {} 
        (for [[k v] ids] (Integer. v)))
      (into {}
        (for [[gov dep off score] deps] 
          [(Integer. gov) (Integer. dep) (Integer. off) (Double. score)]))
    ]))

(defn load- [id]
  (let [path (get-path id)]
    (io/doing-done (str "Loading " id " from " path)
      (case id
        :nmd (into {} (io/parse-tsv path))
        :dict (trie/trie (map #(conj % 1) (io/parse-tsv path)))
        :dm-dict (trie/trie (map #(vector (first %) 1 (rest %)) (io/parse-tsv path)))
        :nmd-g (into {} (io/parse-tsv path))
        :tlm (edu.berkeley.nlp.lm.io.LmReaders/readLmBinary path)

        :dpb (let [[ids deps] (load-dpb-data path)]
               (fn [w1 w2 offset]
                 (deps [(ids w1) (ids w2) offset] 0)))))))

(defmacro load-and-bind [ids & body]
  `(clojure.core/binding ~(vec (mapcat (fn [id] [(symbol (str "norm.data/" (.toUpperCase (name id)))) `(norm.data/load- ~id)]) ids) )
     (do ~@body)))
