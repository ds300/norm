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

(deftest raw-test
  (testing "raw input works yes?"
    (is (every? true? (map = expected_raw (get-stream "raw" "./test/norm/data/io.raw.txt"))))))

(deftest tkn-test
  (testing "tkn input works yes?"
    (is (every? true? (map = expected_tkn (get-stream "tkn" "./test/norm/data/io.tkn.txt"))))))

(deftest json-test
  (testing "json input works yes?"
    (prn (get-stream "json" "./test/norm/data/io.json.txt"))
    (is (every? true? (map = expected_json (get-stream "json" "./test/norm/data/io.json.txt"))))))

