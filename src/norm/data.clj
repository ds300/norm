(ns norm.data
(:require [norm.io :as io]
          [norm.trie :as trie]))

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
])

(doseq [id (map name FILES)]
  (.setDynamic (intern 'norm.data (symbol (.toUpperCase id)))))

(def PATHS (atom {}))

(defn set-path! [id path]
  (swap! PATHS conj [id path]))

(defn set-paths! [dir]
  (doseq [id FILES]
    (set-path! id (str dir "/" (name id)))))


(defn load- [id]
  (let [path (@PATHS id)]
    (io/doing-done (str "Loading " id " from " path)
      (case id
        :nmd (into {} (io/parse-tsv path))
        :dict (trie/trie (map #(conj % 1) (io/parse-tsv path)))
        :dm-dict (trie/trie (map #(vector (first %) 1 (rest %)) (io/parse-tsv path)))
        :nmd-g (into {} (io/parse-tsv path))))))

(defmacro load-and-bind [ids & body]
  `(clojure.core/binding ~(vec (mapcat (fn [id] [(symbol (str "norm.data/" (.toUpperCase (name id)))) `(norm.data/load- ~id)]) ids) )
     (do ~@body)))
