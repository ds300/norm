(ns norm.words-test
  (:require [norm.trie :as trie])
  (:use midje.sweet norm.words))

(fact "`levenshtein` returns the levenshtein distance between two strings"
  (levenshtein "teeth" "teets") => 1
  (levenshtein "jesus" "brian") => 5)

(fact "`common-prefix-length` returns the length of the prefix shared by two strings"
  (common-prefix-length "trees" "try") => 2
  (common-prefix-length "hello" "hello") => 5
  (common-prefix-length "jesus" "brian") => 0)

(fact "`common-suffix-length` returns the length of the suffix shared by two strings"
  (common-suffix-length "bees" "trees") => 3
  (common-suffix-length "smelly" "smelly") => 6
  (common-suffix-length "jesus" "brian") => 0)

(fact "`longest-common-subsequence` returns the length of the longest common subsequence between two strings (not necessarily contiguous)"
  (longest-common-subsequence "mike" "me") => 2
  (longest-common-subsequence "beans" "bees") => 3
  (longest-common-subsequence "jesus" "brian") => 0)

(fact "`remove-repetition` reduces instances of a character being repeated 4 or more times to 3 times, and repetitions of 2 different characters from 3 or more to two."
  (remove-repetition "bananananana") => "banana"
  (remove-repetition "aaaaaaammmmmaaaaaazzzzzzziiiiiinnnnnnngggggg") => "aaammmaaazzziiinnnggg")

(fact "`word-tokenise` returns a lazy seq of word tokens in the given text, ignoring #/@-tags"
  (word-tokenise "hello @user hope ur havin' nice tues #tuesdaygreetings")
  => ["hello" "hope" "ur" "havin'" "nice" "tues"])

(fact "`remove-punct-repetition` is like remove-repetition but only for punctuation"
  (remove-punct-repetition "heeeeeellloooooo!!!!!!!!!") => "heeeeeellloooooo!!!")

(fact "`n-grams` returns n-grams of the given coll as a vector"
  (n-grams 2 "totally") => [[\t \o] [\o \t] [\t \a] [\a \l] [\l \l] [\l \y]]
  (n-grams 3 [:a :b :c :d]) => [[:a :b :c] [:b :c :d]]
  (n-grams 5 [:a :b :c :d]) => [])

(fact "`context-left` gets window_size elements from coll, left of index i"
  (context-left [:a :b :c :d] 2 2) => [:a :b]
  (context-left [:a :b :c :d] 1 0) => [])

(fact "`context-right` gets window_size elements from coll, right of index i"
  (context-right [:a :b :c :d] 2 1) => [:c :d]
  (context-right [:a :b :c :d] 1 3) => [])

(fact "`indexed-context` gets window_size elements from either side of index i in coll"
  (indexed-context [:a :b :c :d :e :f] 2 3) => [[-2 :b] [-1 :c] [1 :e] [2 :f]]
  (indexed-context [:a :b :c :d] 1 3) => [[-1 :c]]
  (indexed-context [] 1 3) => [])

(fact "`raw-confusion-set` gets all words from dict and dm-dict within lex-dist and phon-dist from word, respectively."
  (let [dict (trie/trie [["beats" 1] ["bananas" 3] ["hello" 4]])
        dm-dict (trie/trie [["PNNS" 1 ["bananas" "panninis"]]])]
    (raw-confusion-set dict dm-dict 1 0 "beans") => ["beats"]
    (into #{} (raw-confusion-set dict dm-dict 1 1 "beans")) => #{"beats" "bananas" "panninis"}))

(fact "`lm-ranked-confusion-set` just takes a function that returns a confusion set for a word, then ranks that word based on its context with a language model."
  (let [lm (fn [[w1 w2 w3]] (case w2 "beans" 0.5 "trees" 0.9 "hello" 1.0))
        get-cs (fn [w] ["beans" "trees" "hello"])]
    (lm-ranked-confusion-set lm get-cs ["some" "stupid" "tokens"] 1)
    => ["hello" "trees" "beans"]))
