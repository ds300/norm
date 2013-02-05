(ns norm.trie)

(defn trie
  "construct yoself a trie"
  ([] (norm.trie.Trie.))
  ([args] (into (trie) args)))

(defn freq
  "The frequency of the given string in the given trie"
  [^norm.trie.Trie t s]
  (.tfreq t s))

(defn pfreq
  "The frequency of the given prefix in the given trie"
  [^norm.trie.Trie t s]
  (.freq t s))

