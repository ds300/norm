(ns norm.trie-test
  (:require [norm.trie :as trie])
  (:use midje.sweet))

(facts "about tries"
  (fact "Tries are an immutable map from strings to [freq, data] pairs."
    (let [t (trie/trie [["word" 6 true] ["words" 7 nil]])]
      (keys t) => (contains ["word" "words"])
      (vals t) => (contains [[6 true] [7 nil]])))

  (fact "Tries are also functions of their keys, returning the associated data obejct, if it exists."
    ((trie/trie [["foo" 5 :quux]]) "foo") => :quux)

  (fact "You don't need to specify data objects when you build a trie, but frequencies are required. Seqs are generated in alphabetical order."
    (seq (trie/trie [["hey" 1] ["bar" 4]])) => (contains [["bar" [4 nil]] ["hey" [1 nil]]]))

  (fact "Tries implement clojure.lang.IPersistentCollection"
    (seq (conj (trie/trie [["easy" 3 :foo]]) ["hello" 4 :quux])) => (contains [["easy" [3 :foo]] ["hello" [4 :quux]]]))

  (fact "assoc assigns a data object, dissoc deletes the entry"
    (let [t (trie/trie [["hey" 6] ["yo" 5 :blah]])]
      ((assoc t "hey" :heeey!) "hey") => :heeey!
      ((dissoc t "yo") "yo") => nil))

  (fact "tries can be merged"
    (seq (trie/combine (trie/trie [["hi" 3 :bar]]) (trie/trie [["hey" 90 nil] ["hi" 87 nil]])))
    => (seq (trie/trie [["hi" 90 :bar] ["hey" 90 nil]])))

  (fact "You can specify a function to merge two data objects in the case that a key has non-nil data in both tries."
    ((trie/combine-with into
       (trie/trie [["adjectives" 0 #{"golden" "smelly"}]])
       (trie/trie [["adjectives" 0 #{"incongruous" "epic"}]]))
     "adjectives")
    => #{"golden" "incongruous" "epic" "smelly"})

  (fact "the frequency of any prefix can be obtained with `prefix-freq`"
    (trie/prefix-freq (trie/trie [["yo" 70] ["you" 2000]]) "yo") => 2070)

  (fact "`contains` tells you whether or not a word (i.e. not a prefix) is in the trie"
    (let [t (trie/trie [["beans" 40]])]
      (trie/contains t "beans") => truthy
      (trie/contains t "be") => falsey))

  (fact "`find-nearest` takes an arbitrary string and finds the closest match(es) in the trie, in ascinding order of levenshtein distance."
    (let [t (trie/trie [["beans" 10] ["beats" 15] ["beers" 32] ["meanies" 923]])]
      (trie/find-nearest t "means") => "beans"
      (trie/find-nearest t "beanz" 3) => ["beans" "beats" "beers"]))

  (fact "`find-within` finds all words within a given edit distance."
    (let [t (trie/trie [["beans" 10] ["beats" 15] ["beers" 32] ["meanies" 923]])]
      (into #{} (trie/find-within t "means" 2)) => #{"beans" "meanies" "beats"}))

  (fact "`find-with-prefix` finds all words in the trie which share the given prefix string."
    (let [t (trie/trie [["beans" 1] ["bees" 4] ["hay" 4] ["hey" 3] ["hello" 5]])]
      (into #{} (trie/find-with-prefix t "be")) => #{"beans" "bees"}
      (into #{} (trie/find-with-prefix t "he")) => #{"hey" "hello"}))

  (fact "`find-with-substr` finds all words in the trie which contain (contiguously) the given  substring."
    (let [t (trie/trie [["beans" 1] ["bees" 4] ["hay" 4] ["hey" 3] ["heeey" 5]])]
      (into #{} (trie/find-with-substr t "ee")) => #{"heeey" "bees"}))

  (fact "`find-with-chars` finds all words in the trie which contain the given substring."
    (let [t (trie/trie [["beans" 1] ["bees" 4] ["hay" 4] ["easy" 3] ["see?" 5]])]
      (into #{} (trie/find-with-chars t "es")) => #{"beans" "bees" "easy"}))

  (fact "`find-with-suffix` finds all words in the trie which share the given suffix."
    (let [t (trie/trie [["beans" 1] ["bees" 4] ["hay" 4] ["easy" 3] ["see?" 5]])]
      (into #{} (trie/find-with-suffix t "y")) => #{"hay" "easy"})))

