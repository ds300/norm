(ns norm.trie-test
  (:use clojure.test norm.trie))

(def entries [
  ["Hello" 5]
  ["hello" 4 {:good true}]
  ["other" 3]
  ["word" 6]
  ["now" 5]
  ["noun" 9]
])

(deftest test-trie
  (testing "constructor"
    (let [t (trie entries)]
      (is t)
      (is (:good (t "hello")))
      (is (= 3 (freq t "other")))
      (is (= 14 (prefix-freq t "no")))))

  (testing "adding and removing items"
    (let [t (trie entries)
          t1 (assoc t "good" {:good true})
          t2 (dissoc t "hello")
          t3 (assoc t1 "good" false)
          t4 (conj t ["word" 6])]
      (map #(is (.verifyDepths %)) [t, t1, t2, t3, t4])
      (is (:good (t1 "good")))
      (is (zero? (freq t2 "hello")))
      (is (contains t3 "good"))
      (is (not (t3 "good")))
      (is (= 12 (freq t4 "word"))))))

; TODO: more tests. fuzzy matching, merging, etc.