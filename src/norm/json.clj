(ns norm.json
  "a stream json parser"
  (:require [clojure.java.io :as jio]))



(defn char-seq [^java.io.Reader rdr]
  (let [c (.read rdr)]
    (if (>= c 0)
      (cons (char c) (lazy-seq (char-seq rdr)))
      (.close rdr))))



(def symbols #{\[ \] \: \{ \} \,})



(defn- parse-number [[c & cs]]
  "man, this is ugly clojue"
  (let [buff (StringBuilder.)
        remaining (atom cs)]
    (.append buff c)
    (loop [[d & ds] @remaining]
      (if (Character/isDigit d)
        (do (.append buff d) (recur ds))
        (reset! remaining (cons d ds))))
    (if (#{\. \E \e} (first @remaining))
      (do
        (when (= \. (first @remaining))
          (.append buff \.)
          (loop [[d & ds] (rest @remaining)]
            (if (Character/isDigit d)
              (do (.append buff d) (recur ds))
              (reset! remaining (cons d ds)))))
        (when (#{\E \e} (first @remaining))
          (.append buff \e)
          (reset! remaining (rest @remaining))
          (when (#{\+ \-} (first @remaining))
            (.append buff (first @remaining))
            (reset! remaining (rest @remaining)))
          (loop [[d & ds] @remaining]
            (println (str buff))
            (if (Character/isDigit d)
              (do (.append buff d) (recur ds))
              (reset! remaining (cons d ds)))))
        [(Double/valueOf (str buff)) @remaining])
      [(Long/valueOf (str buff)) @remaining])))

(defn- parse-string [[c d & cs] ^StringBuilder acc]
  (cond
    (= c \")
      [(str acc) (cons d cs)]
    (= c \\)
      (let [[to_append remaining]
              (case d
                \" [\" cs]
                \\ [\\ cs]
                \/ [\/ cs]
                \b [\backspace cs]
                \f [\formfeed cs]
                \n [\newline cs]
                \r [\return cs]
                \t [\tab cs]
                \u [(char (Integer/valueOf (apply str (take 4 cs)) 16)) (drop 4 cs)])]
        (.append acc to_append)
        (recur remaining acc))
    (Character/isISOControl c)
      (throw (Exception. "control character in string literal"))
    :else
      (do (.append acc c) (recur (cons d cs) acc))))


(defn- tokens [[c & cs]]
  (when c
    (cond
      (Character/isWhitespace c)
        (recur cs)
      (or (Character/isDigit c) (= \- c))
        (let [[n remaining_chars] (parse-number (cons c cs))]
          (cons ["num" n] (lazy-seq (tokens remaining_chars))))
      (symbols c)
        (cons ["sym" c] (lazy-seq (tokens cs)))
      (= c \")
        (let [[s remaining_chars] (parse-string cs (StringBuilder.))]
          (cons ["str" s] (lazy-seq (tokens remaining_chars))))
      (and (= c \n) (= (take 3 cs) '(\u \l \l)))
        (cons ["null" nil] (lazy-seq (tokens (drop 3 cs))))
      (and (= c \t) (= (take 3 cs) '(\r \u \e)))
        (cons ["bool" true] (lazy-seq (tokens (drop 3 cs))))
      (and (= c \f) (= (take 4 cs) '(\a \l \s \e)))
        (cons ["bool" false] (lazy-seq (tokens (drop 4 cs))))
      :else (throw (Exception. (str "Bad character"))))))

