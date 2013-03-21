(ns norm.io-test
  (:use midje.sweet norm.io))

(defn string-reader [string]
  (-> string
    java.io.StringReader.
    java.io.BufferedReader.))

(facts "about raw streams"
  (fact "raw strams take a BufferedReader over newline-separated tweets and tokenises them, putting the result in a map with the original text"
    (first (get-stream "raw" (string-reader "a tweet\n")))
    => {"text" "a tweet", "tokens" ["a" "tweet"]})

  (fact "it should also work without trailing newline"
    (first (get-stream "raw" (string-reader "a tweet")))
    => {"text" "a tweet", "tokens" ["a" "tweet"]})

  (fact "tweet objects are returned as a lazy seq"
    (get-stream "raw" (string-reader "a tweet\nanother tweet!"))
    => (lazy-seq [
                   {"text" "a tweet", "tokens" ["a" "tweet"]}
                   {"text" "another tweet!", "tokens" ["another" "tweet" "!"]}
                 ])))

(facts "about tkn streams"
  (fact "tkn streams take a BufferedReader over newline-separated tokens and concats them into tweet objects, with reconstituted text"
    (first (get-stream "tkn" (string-reader "a\ntweet\n")))
    => {"text" "a tweet", "tokens" ["a" "tweet"]})

  (fact "it should also work without trailing newline"
    (first (get-stream "tkn" (string-reader "a\ntweet")))
    => {"text" "a tweet", "tokens" ["a" "tweet"]})
  
  (fact "tweet objects are returned as a lazy seq, but detokenisation is not intelligent. i.e. tokens are joined with spaces such that [\"hello\" \"!\"] => [\"hello !\"]"
    (get-stream "tkn" (string-reader "a\ntweet\n\nanother\ntweet\n!"))
    => (lazy-seq [
                   {"text" "a tweet", "tokens" ["a" "tweet"]}
                   {"text" "another tweet !", "tokens" ["another" "tweet" "!"]}
                 ])))

(facts "about json streams"
  (fact "json streams take a BufferedReader over a json list of json tweet objects which must contain atleast one of the keys \"text\" or \"tokens\""
    (first (get-stream "json" (string-reader "[{\"text\":\"a tweet\"}]")))
    => {"text" "a tweet", "tokens" ["a" "tweet"]})
  (fact "if tokens are provided, the text field doesn't get tokenised"
    (first (get-stream "json" (string-reader "[{\"text\":\"a tweet\", \"tokens\": [\"foo\", \"bar\", \"quux\"]}]")))
    => {"text" "a tweet", "tokens" ["foo" "bar" "quux"]})
  (fact "similarly, if tokens are provided without text, the text field becomes the detokenised form of the given tokens."
    (first (get-stream "json" (string-reader "[{\"tokens\": [\"foo\", \"bar\", \"quux\", \"!\"]}]")))
    => {"text" "foo bar quux !", "tokens" ["foo" "bar" "quux" "!"]})
  (fact "tweet objects are returned as a lazy seq"
    (get-stream "json" (string-reader "[{\"tokens\": [\"foo\", \"bar\", \"quux\", \"!\"]}, {\"text\":\"a tweet\"}]"))
    => (lazy-seq [{"text" "foo bar quux !", "tokens" ["foo" "bar" "quux" "!"]}
                  {"text" "a tweet", "tokens" ["a" "tweet"]}])))


(facts "about spit-tsv"
  (fact "spit-tsv takes a java.io.Writer and a seq of seqs, and prints each inner seq as a tab-separated list followed by a linefeed."
    (let [w (java.io.StringWriter.)]
      (spit-tsv w [[:foo :bar :quux] [:cheese :is :the :best]])
      (.toString w))
    => ":foo\t:bar\t:quux\n:cheese\t:is\t:the\t:best\n")
  
  (fact "spit-tsv should not be used to encode strings which contain newlines or tabs"
    (let [w (java.io.StringWriter.)]
      (spit-tsv w [["this\nis" "bad\t!"]])
      (vec (parse-tsv (string-reader (.toString w)))))
    => [["this"] ["is" "bad" "!"]]))


(facts "about parse-tsv"
  (fact "parse-tsv takes anything which can be used by clojure.java.io/reader and returns
         a lazy seq of vectors."
    (parse-tsv (string-reader "hey\tthere\nmr\tsir"))
    => (lazy-seq [["hey" "there"] ["mr" "sir"]]))

  (fact "parse-tsv takes optional functions which are positionally called on the resulting vectors"
    (parse-tsv (string-reader "teeth\t24\neyes\t2") identity #(Integer. %))
    => (lazy-seq [["teeth" 24] ["eyes" 2]])

    (doall (parse-tsv (string-reader "teeth\t24\neyes\t2\nhair\tfoo") identity #(Integer. %)))
    => (throws NumberFormatException)))

