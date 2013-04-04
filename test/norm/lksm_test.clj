(ns norm.lksm-test
  (:require [norm.trie :as trie])
  (:use midje.sweet norm.train.lksm))

(def tkns ["hello" "there" "," "my" "good" "sir" "!"])

(def dict (trie/trie [["hello" 1] ["there" 1] ["my" 1] ["good" 1] ["sir" 1]]))

(fact "`extract-positive-features` takes a dict, some tokens, and an index. It returns IV words around the token at the given index, within a context window of 3."
  (extract-positive-features dict tkns 3)
  => [[true "hello" "my" -3]
      [true "there" "my" -2]
      [true "good" "my" 1]
      [true "sir" "my" 2]])

(fact "`derive-negative-features` takes some positive features and a confusion set, replacing dependents with elements from the confusion set."
  (derive-negative-features
    [[true "hello" "my" -3]
     [true "there" "my" -2]]
    ["moo" "midje"])
  => [[false "hello" "moo" -3]
      [false "hello" "midje" -3]
      [false "there" "moo" -2]
      [false "there" "midje" -2]])

(fact "`extract-features!` extracts feature vectors from a line of text."
  (extract-features!
    dict
    (fn [w off] 0.5)
    (fn [a] (if (vector? a) (second a) a))
    identity
    (fn [tkns i] ["moo"])
    identity
    "hello there, sir!")
  => [[true "there" "hello" 1 0.5]
      [true "sir" "hello" 3 0.5]
      [false "there" "moo" 1 0.5]
      [false "sir" "moo" 3 0.5]
      [true "hello" "there" -1 0.5]
      [true "sir" "there" 2 0.5]
      [false "hello" "moo" -1 0.5]
      [false "sir" "moo" 2 0.5]
      [true "hello" "sir" -3 0.5]
      [true "there" "sir" -2 0.5]
      [false "hello" "moo" -3 0.5]
      [false "there" "moo" -2 0.5]])

(fact "`encode-feature-vector` takes a feature vector and encodes it to libsvm format"
  (encode-feature-vector 1 [true 34 23 9 0.2345])
  => "1 1:0.2345 9:1 23:1 34:1\n")

