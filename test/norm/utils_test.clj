(ns norm.utils-test
  (:use midje.sweet norm.utils))

(fact "`at-least` returns true iff n elements in coll satisfy pred"
  (at-least 3 odd? [1 4 6 5 8 9]) => truthy
  (at-least 4 even? [1 4 6 5 8 9]) => falsey)

(fact "`pmap-chunked` maps f over coll in chunks of size n, to reduce concurrency overhead"
  (pmap-chunked 3 inc (range 10)) => (range 1 11))

(fact "`pmapcat` is like mapcat, but runs f concurrently, using pmap"
  (vec (pmapcat reverse [[1 2 3] [4 5 6] [7 8 9]])) => [3 2 1 6 5 4 9 8 7])

(fact "`pmapcat-chunked` is like pmapcat, but uses pmap-chunked"
  (vec (pmapcat-chunked 2 reverse [[1 2 3] [4 5 6] [7 8 9]])) => [3 2 1 6 5 4 9 8 7])

(fact "`unchunk` takes a chunked-seq (or any lazy seq, really) and ensures that, when consumed, only one item gets processed at a time. This is important to avoid unwanted side-effects."
  (let [acc (atom 0)
        f   (fn [n] (swap! acc + n))]
    (second (map f (range)))
    (= 1 @acc) => falsey)
  (let [acc (atom 0)
        f   (fn [n] (swap! acc + n))]
    (second (map f (unchunk (range))))
    (= 1 @acc) => truthy))

(fact "`pmapall` is like pmap, but eager. i.e. it'll just keep processing the collection in a fixed-size threadpool regardless of how many elements have been consumed."
  (pmapall inc (range 10)) => (range 1 11))

(fact "`pmapall-chunked` is like pmap-chunked, but uses pmapall instead of pmap."
  (pmapall-chunked 3 inc (range 10)) => (range 1 11))

(facts "about `unique-id-getter`"
  (fact "`unique-id-getter` returns a function that assignes unique integer ids to elements."
    (let [id (unique-id-getter)]
      (id "hello") => 0
      (id "there") => 1
      (id "sir")   => 2))

  (fact "you can specify a start value"
    (let [id (unique-id-getter 5)]
      (id "hello") => 5
      (id "there") => 6
      (id "sir")   => 7))

  (fact "if you supply two arguments, the second is returned when the first has not been given an id."
    (let [id (unique-id-getter)]
      (id "hello") => 0
      (id "hello" :not-found) => 0
      (id "there" :not-found) => :not-found))

  (fact "if you supply no arguments, the current state of the underlying map is returned"
    (let [id (unique-id-getter)]
      (id "hello")
      (id "there")
      (id "sir")
      (id) => {"hello" 0 "there" 1 "sir" 2})))

(fact "`counted-fn` takes a function f and an atom holding a number, and returns a version of f such that the number in the atom is increased each time it is invoked."
  (let [c (atom 0)
        f (counted-fn inc c)]
    (map f (range 10)) => (map inc (range 10))
    @c => 10))

(facts "about `map-counter`"
  (fact "`map-counter` returns a function with args [elem i] which increases a count for elem by i. Passing only elem returns the current count."
    (let [mc (map-counter)]
      (doall (map mc [1 1 2 1 2 3 1 2 3 4 1 2 3 4 5] (repeat 1)))
      (mc 1) => 5
      (mc 2) => 4
      (mc 3) => 3
      (mc 4) => 2
      (mc 5) => 1
      (mc 6) => nil))

  (fact "when passed no args, the current state of the underlying map is returned."
    (let [mc (map-counter)
          nums [1 1 2 1 2 3 1 2 3 4 1 2 3 4 5]]
      (doall (map mc nums (repeat 1)))
      (mc) => (frequencies nums))))

(fact "`atomised-map-counter` is just like map-counter but numbers are stored in atoms."
  (let [mc (atomised-map-counter)
        nums [1 1 2 1 2 3 1 2 3 4 1 2 3 4 5]]
    (doall (map mc nums (repeat 1)))
    (mc 1) => 5
    (mc 2) => 4
    (mc 3) => 3
    (mc 4) => 2
    (mc 5) => 1
    (mc 6) => nil
    (update-with deref (mc)) => (frequencies nums)))

(fact "`update-with` maps f over the vals in the given map m."
  (update-with not {:likes-cats true :likes-dogs false}) => {:likes-cats false :likes-dogs true})

(fact "`take-percent` takes a collection of size n and returns n/100 * p items"
  (take-percent 20 (range 20)) => (range 4)
  (take-percent 0 (range 20)) => ()
  (take-percent 100 (range 20)) => (range 20))

(facts "about `indexify`"
  (fact "it returns a lazy seq of the elements in coll with a positional index"
    (indexify [:a :b :c]) => [[0 :a] [1 :b] [2 :c]]
    (indexify []) => [])

  (fact "you can specify a starting index"
    (indexify 100 [:a :b :c]) => [[100 :a] [101 :b] [102 :c]]))

(fact "`flat` is like clojure.core/flatten, but also flattens maps."
  (flat [:hey {:there "sir"} [:you [:look] :pretty] :funny])
  => [:hey :there "sir" :you :look :pretty :funny]
  (flat [:hey {:there ["!" [:nice :jeans]]} :lol])
  => [:hey :there "!" :nice :jeans :lol])

(fact "`map-merge` is like clojure.core/merge, but works recursively"
  (map-merge {:a {:b {:c "yo"}}} {:c "hi" :a {:foo "quux"}})
  => {:a {:b {:c "yo"} :foo "quux"} :c "hi"}
  (map-merge {} {}) => {}
  (map-merge {} {:a "a"}) => {:a "a"})
