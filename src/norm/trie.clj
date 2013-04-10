(ns norm.trie)

(defn trie
  "construct yoself a trie"
  ([] (norm.jvm.Trie.))
  ([args] (into (trie) args)))

(defn contains
  "returns true if t contains s, false otherwise"
  [t s]
  (.contains t s))

(defn freq
  "The frequency of the given string in the given trie"
  [^norm.jvm.Trie t s]
  (.tfreq t s))

(defn prefix-freq
  "The frequency of the given prefix in the given trie"
  [^norm.jvm.Trie t s]
  (.freq t s))

(defn combine
  "Take some tries and merges them, frequencies are combined.
   Where identical entries both have data objects, the rightmost is used."
  [& ts]
  (when (some identity ts)
    (reduce #(.merge %1 %2 nil) ts)))

(defn combine-with
  "Take some tries and merge them, frequencies are combined.
   Where identical entries both have data objects, the result of f
   applied to both is used"
  [f & ts]
  (when (some identity ts)
    (reduce #(.merge %1 %2 f) ts)))

(defn find-nearest
  ([t s] (first (find-nearest t s 1)))
  ([t s n] (into [] (.findNearest t s n (Integer/MAX_VALUE)))))

(defn find-within
  [t s d]
  (into [] (.findNearest t s 0 d)))

(defn find-with-prefix [t prefix]
  (into [] (.findWithPrefix t prefix)))

(defn find-with-substr [t substr]
  (into [] (.findContaining t substr true)))

(defn find-with-chars [t substr]
  (into [] (.findContaining t substr false)))

(defn find-with-suffix [t suffix]
  (into [] (.findWithSuffix t suffix)))
