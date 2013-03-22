(ns norm.alise-test
  (:require [norm.trie :as trie]
            [norm.words :as words])
  (:use midje.sweet norm.alise))

(def dict (trie/trie [["hello" 1] ["there" 1] ["sir" 1]]))

(def sentence ["well" "hello" "there" "mr" "sir" "!"])

(fact "`form-exemplars` gets iv contextual features"
  (into #{}
    (form-exemplars dict "good" sentence 3))
  => #{["hello" "good" -2] ["there" "good" -1] ["sir" "good" 1]})

(fact "`most-frequent` returns the most frequent item in coll, or nil if no element occurs more often than all others."
  (most-frequent [:a :b :c :a :b :b]) => :b
  (most-frequent [:a :b :c :a :b]) => nil
  (most-frequent []) => nil)

(defn lksm [[w1 w2 offset]]
  (case w1
    "hello" "pos"
    "there" "neg"
    "sir"   "pos"
    "neg"))

(fact "`predict` takes a function which returns pos or neg class for an exemplar, and a seq of exemplars. It returns the most-frequent class or neg."
  (predict lksm [["hello" "good" -2] ["there" "good" -1] ["sir" "good" 1]])
  => "pos"

  (predict lksm [["hello" "good" -2] ["there" "good" -1] ["massive" "good" 1]])
  => "neg"

  (predict lksm [])
  => "neg")

(defn lksm2 [[w1 w2 offset]]
  (case w2
    "good" (case w1
             "hello" "pos"
             "there" "neg"
             "sir"   "pos"
                     "neg")
    "bad"  (case w1
             "hello" "neg"
             "there" "pos"
             "sir"   "neg"
                     "neg")
    "chaotic-nuetral"  (case w1
                         "hello" "pos"
                         "there" "neg"
                         "sir"   "pos"
                                 "pos")))

(fact "`ill-formed?` returns true if a word in context is deemed to be ill formed."
  (ill-formed? dict lksm2 1 ["good" "bad" "chaotic-nuetral"]
    ["well" "hello" "there" "mr" "sir" "!"]
    3)
  => true
  (ill-formed? dict lksm2 1 ["good" "bad" "chaotic-nuetral"]
    ["well" "hello" "there" "mr" "baggins" "!"]
    3)
  => false)

(fact "`rank-by` returns cs ranked, in groups, by #(f original %)"
  (rank-by words/levenshtein ["beans" "beers" "pleats" "bananas"] "beats")
  => [[0 ["beans"]] [1 ["beers" "pleats"]] [2 ["bananas"]]])

(fact "`choose-candidate` picks a candidate from cs based on similarity to orig"
  (choose-candidate ["beans" "beers" "pleats" "bananas"] "beats")
  => "beans")


