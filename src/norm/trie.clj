(ns norm.trie)

(defn trie
  "construct yoself a trie"
  ([] (norm.trie.Trie.))
  ([args] (into (trie) args)))

(defn freq
  "The frequency of the given string in the given trie"
  [^norm.trie.Trie t s]
  (.tfreq t s))

(defn prefix-freq
  "The frequency of the given prefix in the given trie"
  [^norm.trie.Trie t s]
  (.freq t s))

(defn merge
  "Take some tries and merge them, frequencies are combined.
   Where identical entries both have data objects, the rightmost is used."
  [& ts]
  (when (some identity ts)
    (reduce #(.merge %1 %2 nil) ts)))

(defn merge-with
  "Take some tries and merge them, frequencies are combined.
   Where identical entries both have data objects, the result of f
   applied to both is used"
  [f & ts]
  (when (some identity ts)
    (reduce #(.merge %1 %2 f) ts)))

(defn find-nearest
  ([t s] (find-nearest t s 1))
  ([t s n] (into [] (.findNearest t s n 0))))

(defn find-within
  [t s d]
  (into [] (.findNearest t s 0 d)))

(defn find-with-prefix [t prefix]
  (.findWithPrefix t prefix))

(defn find-with-substr [t substr]
  (.findContaining t substr true))

(defn find-with-chars [t substr]
  (.findContaining t substr false))

(defn find-with-suffix [t suffix]
  (.findWithSuffix t suffix))
