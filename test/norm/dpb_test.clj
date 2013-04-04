(ns norm.dpb-test
  (:require [norm.io :as io]
            [norm.trie :as trie])
  (:use midje.sweet norm.train.dpb))

(def in (java.io.BufferedReader.
          (java.io.StringReader.
            (slurp "test/norm/data/nyt.test.xml"))))

(fact "`documents` takes a reader, and returns a lazy seq of document elements."
  (def docs (documents in))
  (instance? clojure.lang.LazySeq docs) => truthy
  (:tag (first docs)) => :DOC
  (count docs) => 2)

(fact "`sentences` takes a doc and returns the sentences in it."
  (def sents (mapcat sentences docs))
  (:tag (first sents)) => :sentence
  (count sents) => 4)

(fact "`tokens` returns tokens from a sentence in vector format."
  (tokens (first sents))
  => ["hello" "there" "my" "good" "sir" "!"]
  (tokens (second sents))
  => ["how" "are" "you" "?"])

(def dict (trie/trie [["hello" 1] ["there" 1] ["sir" 1] ["how" 1] ["you" 1]]))

(fact "`extract-untyped-deps!` stores dependencies from a sentence, wherein the dependent is OOV and the governor is IV."
  (let [store (atom #{})
        store-fn (fn [[gov offset :as elem]] (swap! store conj elem))]
    (extract-untyped-deps! dict identity identity store-fn (first sents))
    (extract-untyped-deps! dict identity identity store-fn (second sents))
    @store => #{["sir" 1] ["sir" 2] ["how" -1]}))

