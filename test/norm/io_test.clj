(ns norm.io-test
  (:use clojure.test
        norm.io))

(def expected_raw [
  {
    "text" "this is a tweet! hahaha"
    "tokens" ["this" "is" "a" "tweet" "!" "hahaha"]
  }
  {
    "text" "so is this! #lolzorz"
    "tokens" ["so" "is" "this" "!" "#lolzorz"]
  }
  {
    "text" "RT: @coffee_face u is well tasty face #luvcoffee"
    "tokens" ["RT" ":" "@coffee_face" "u" "is" "well" "tasty" "face" "#luvcoffee"]
  }
])

(def expected_tkn [
  {
    "text" "this is a tweet ! hahaha"
    "tokens" ["this" "is" "a" "tweet" "!" "hahaha"]
  }
  {
    "text" "so is this ! #lolzorz"
    "tokens" ["so" "is" "this" "!" "#lolzorz"]
  }
  {
    "text" "RT : @coffee_face u is well tasty face #luvcoffee"
    "tokens" ["RT" ":" "@coffee_face" "u" "is" "well" "tasty" "face" "#luvcoffee"]
  }
])

(def expected_json [
  {
    "text" "this is a tweet! hahaha"
    "tokens" ["this" "is" "a" "tweet" "!" "hahaha"]
  }
  {
    "text" "so is this! #lolzorz"
    "tokens" ["so" "is" "this" "!" "#lolzorz"]
    "another_field" "i get carried along"
  }
  {
    "text" "RT: @coffee_face u is well tasty face #luvcoffee"
    "tokens" ["These" "tokens" "be" "different" "yo" "!"]
  }
  {
    "text" "this just tokens"
    "tokens" ["this" "just" "tokens"]
  }
])

(defn equal-seqs? [& seqs]
  (every? true?
    (apply (partial map =) seqs)))

(deftest raw-test
  (testing "raw input works yes?"
    (open [:r in "./test/norm/data/io.raw.txt"]
      (is (equal-seqs? expected_raw (get-stream "raw" in))))))

(deftest tkn-test
  (testing "tkn input works yes?"
    (open [:r in "./test/norm/data/io.tkn.txt"]
      (is (equal-seqs? expected_tkn (get-stream "tkn" in))))))

(deftest json-test
  (testing "json input works yes?"
    (open [:r in "./test/norm/data/io.json.txt"]
    (is (equal-seqs? expected_json (get-stream "json" in))))))

(deftest spit-tsv-test
  (testing "spit-tsv works yes?"
    (let [wrt (java.io.StringWriter.)
          data '(["some value" "another_value" "one more value"] [3 6 22])
          expected "some value\tanother_value\tone more value\n3\t6\t22\n"]
      (spit-tsv wrt data)
      (is (=
            (str wrt)
            expected)))))

(deftest parse-tsv-test
  (testing "parse-tsv works yes?"
    (let [rdr (java.io.StringReader. "monkeys are\t78\ntotally\t9304\tawesome\t4687\n")
          expected [["monkeys are" 78] ["totally" 9304 "awesome" "4687"]]]
      (is (equal-seqs?
            expected
            (parse-tsv rdr identity #(Integer. %)))))))

