(ns norm.json-test
  (:import [java.io StringReader])
  (:use midje.sweet norm.json))

(fact "`char-seq` returns a lazy seq of chars from the reader."
  (char-seq (StringReader. "abcdefg hetc..."))
  => [\a \b \c \d \e \f \g \space \h \e \t \c \. \. \.])

(fact "`parse-number` takes a char-seq and returns a number, along with the remaining chars."
  (parse-number [\1 \. \5 \e \1 \0 \space \e \t \c])
  => [1.5e10 [\space \e \t \c]]
  (parse-number [\5 \0])
  => [50 []]
  (parse-number [\5 \e \1 \0 \e])
  => (throws NumberFormatException))

(fact "`parse-string` takes a char-seq and a StringBuilder and returns a json string, along with remaining chars."
  (parse-string [\a \space \s \t \r \\ \n \i \n \g \" \e \t \c] (StringBuilder.))
  => ["a str\ning" [\e \t \c]])

(fact "`tokens` takes a char-seq and returns a lazy seq of tokens"
  (tokens (char-seq (StringReader. "true false \n null \"a string\" , : -70 ")))
  => [["val" true] ["val" false] ["val" nil] ["val" "a string"] ["sym" \,] ["sym" \:] ["val" -70]]
  (tokens (char-seq (StringReader. "blah")))
  => (throws Exception))


(defn tkns [s] (tokens (char-seq (StringReader. s))))
(fact "`parse-value` takes a token seq and returns a value, plus remaining tokens."
  (parse-value (tkns "{\"a\" : \"map\"} true false"))
  => [{"a" "map"} [["val" true], ["val" false]]]

  (parse-value (tkns "true false"))
  => [true [["val" false]]]

  (parse-value (tkns "[1024e2 45]"))
  => [[1024e2 45] nil])

(fact "`lazy-list` returns a lazy seq of the items in the json list encoded by the given token stream."
  (lazy-list (tkns "50, 20, \"teeth\", null]"))
  => [50, 20, "teeth", nil])


(fact "`objects-in` returns a lazy seq of the top-level json objects in the given reader. This isn't legit json stuff, just a convenience."
  (objects-in (StringReader. "{\"foo\": 4}, {\"quux\":[09 08 0000006]}"))
  => [{"foo" 4} {"quux" [9 8 6]}])
  
