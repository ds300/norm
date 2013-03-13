(ns norm.data
(:import [java.io IOException]
         [de.bwaldvogel.liblinear Linear FeatureNode])
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
  (.setDynamic (intern 'norm.data id))
  (config/declare-opt! :data :paths (keyword (clojure.string/lower-case id))))


(defn get-path
  "get the path of a particular file (id should be a keyword)"
  [id]
  (let [p (config/opt :data :paths id)]
    (if (= p :undefined)
      (str (config/opt :data :dir) "/" (name id))
      p)))

(defn atoi "make an integer of a" [a]
  (Integer. a))

(defn atod "make a double of a" [a]
  (Double. a))

(defn load-dpb-data
  "load dependency bank data from the specified path. returns
  a tuple of the form [word_id_map, dependency_bank]"
  [path]
  (into {} 
    (for [[word & offest_scores] (apply
                                   (partial io/parse-tsv path identity)
                                   (flatten (repeat [atoi atod])))] 
      [word (apply hash-map offest_scores)])))

(defn load-dpb
  "loads the dependency bank at the specified path.
  returns a fn which takes a governor, a dependent, and an offset
  and returns the triple's score in the dependency bank"
  [path]
  (let [data (load-dpb-data path)]
   (fn [w offset]
     (or (get-in data [w offset]) 0))))

(defn load-lksm [path]
  (let [model (Linear/loadModel (java.io.File. path))
        f-ids (into {} (io/parse-tsv (str path "-f-ids") read-string read-string))
        iv-ids (into {} (io/parse-tsv (str path "-iv-ids") identity read-string))]
    (fn [[gov dep off]]
      (let [v (sort (filter identity (map f-ids [[0 (iv-ids gov)] [1 (iv-ids dep)] off])))]
        ({1.0 "pos" 0.0 "neg"}
          (Linear/predict
            model
            (into-array FeatureNode 
              (mapv #(FeatureNode. % 1.0) v)
            )))))))

(defn load-
  "loads the file specified by the id."
  [id]
  (let [path (get-path id)]
    (io/doing-done (str "Loading " id " from " path)
      (case id
        :nmd (into {} (io/parse-tsv path))
        :dict (trie/trie (map #(conj % 1) (io/parse-tsv path)))
        :dm-dict (trie/trie (map #(vector (first %) 1 (rest %)) (io/parse-tsv path)))
        :tlm (edu.berkeley.nlp.lm.io.LmReaders/readLmBinary path)
        :lksm (load-lksm path)
        :dpb (load-dpb path)))))

(defn can-read?
  "Returns true if the location specified by the path is readable."
  [path]
  (.canRead (java.io.File. path)))

(defn can-write!?
  "tries to create and then delete a file at the location specified by
  path. If successful, returns true."
  [path]
  (try (let [f (java.io.File. path)]
         (io/open [:w out f] (.write out "hello\n"))
         (.delete f)
         true)
    (catch IOException e (do false))))

(defn- verify-paths! [fail_header pred ids]
  (when-let [bad_ids (seq (filter (comp not pred get-path) ids))] 
    (println fail_header)
    (doseq [p (map get-path ids)]
      (println "\t" p))
    (shutdown-agents)
    (System/exit 1)))

(defn verify-readable!
  "checks that the files with the given ids are readable."
  [& ids]
  (verify-paths! "Unable to read from:" can-read? ids))

(defn verify-writeable!
  "checks that the files with the given ids are writeable."
  [& ids]
  (verify-paths! "Unable to write to:" can-write!? ids))



(defmacro load-and-bind
  "loads the files with the given ids, binds them to data/FILE where FILE
  is the same as the id but with no colon and uppercase. Then executes body."
  [ids & body]
  `(clojure.core/binding ~(vec (mapcat (fn [id] [(symbol (str "norm.data/" (.toUpperCase (name id)))) `(norm.data/load- ~id)]) ids) )
     (do ~@body)))


