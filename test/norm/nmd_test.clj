(ns norm.nmd-test
  (:require [norm.trie :as trie])
  (:use midje.sweet norm.train.nmd))

(def beans (java.io.BufferedReader.
       (java.io.StringReader.
"beans
 beans, the
 beans, the musical
 beans, the musical fruit!")))

(fact "`count-corpus-words!` returns a seq of [word, freq] pairs from a BufferedReader"
  (into {} (count-corpus-words! beans))
  => {
      "beans" 4
      "the" 3
      "musical" 2
      "fruit" 1
     })

(def dict (trie/trie [["beans" 1] ["the" 1]]))

(fact "`stratify-counted-words` takes a seq of [word, freq] pairs and puts IV words in a trie, and OOV words which satisfy a predicate in a list."
  (let [[oov_words iv_trie]  (stratify-counted-words!
                                dict
                                (fn [[w f]] (> f 1))
                                identity
                                [["beans" 4]
                                  ["the" 3]
                                  ["musical" 2]
                                  ["fruit" 1]])]
    
  oov_words => ["musical"]
  (into #{} (seq iv_trie)) => #{["beans" [4 nil]] ["the" [3 nil]]}))


(def grams (mapv vec (partition 2 1 ["beans" "," "the" "musical" "fruit" "!"])))

(fact "`context-left` returns indexed n-gram context."
  (into #{} (context-left 2 2 grams 3))
  => #{[-1 "," "the"] [-2 "beans" ","]})


(fact "`context-right` returns indexed n-gram context (but from the right)"
  (context-right 2 grams 3)
  => [[1 "fruit" "!"]])
