(ns norm.json
  "a stream json parser"
  (:require [clojure.java.io :as jio]))



(defn char-seq [^java.io.Reader rdr]
  (let [c (.read rdr)]
    (if (>= c 0)
      (cons (char c) (lazy-seq (char-seq rdr)))
      (.close rdr))))



(def SYMBOLS #{\[ \] \: \{ \} \,})

(def NUM_CHARS #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \. \e \E})

(defn- parse-number [cs]
  (let [[numchars remaining_chars] (split-with NUM_CHARS cs)
         dec? (some #{\e \E \.} numchars)
         numstr (apply str numchars)
         num (if dec?
               (Double/valueOf numstr)
               (Long/valueOf numstr))]
    [num remaining_chars]))

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
          (cons ["val" n] (lazy-seq (tokens remaining_chars))))
      (SYMBOLS c)
        (cons ["sym" c] (lazy-seq (tokens cs)))
      (= c \")
        (let [[s remaining_chars] (parse-string cs (StringBuilder.))]
          (cons ["val" s] (lazy-seq (tokens remaining_chars))))
      (and (= c \n) (= (take 3 cs) '(\u \l \l)))
        (cons ["val" nil] (lazy-seq (tokens (drop 3 cs))))
      (and (= c \t) (= (take 3 cs) '(\r \u \e)))
        (cons ["val" true] (lazy-seq (tokens (drop 3 cs))))
      (and (= c \f) (= (take 4 cs) '(\a \l \s \e)))
        (cons ["val" false] (lazy-seq (tokens (drop 4 cs))))
      :else (throw (Exception. (str "Bad character"))))))

(declare parse-list)
(declare parse-object)

(defn parse-value [[type val] & ts]
  (case type
    "val" [val ts]
    "sym"
      (case val
        \{ (parse-object ts)
        \[ (parse-list ts))))

(defn parse-list [tkns]
  (loop [[[type val] & ts] tkns acc (transient [])]
    (case val
      \] [(persistent! acc) ts]
      \, (recur ts acc)
      (let [[item remaining_tkns] (parse-value (cons [type val] ts))]
        (recur remaining_tkns (conj! acc item))))))

(defn parse-object [tkns]
  (loop [[[type val] & [[_ colon] & ts] :as others] tkns acc (transient {})]
    (cond
      (= val \}) [(persistent! acc) others]
      (= val \,) (recur others acc)
      (and (string? val) (= colon \:))
        (let [[item remaining_tkns] (parse-value ts)]
          (recur remaining_tkns (conj! acc [val item]))))))

(defn lazy-list [[[type val] & ts :as tkns]]
  (if (= type "sym")
    (case val \, (recur ts) \] nil)
    (let [[item remaining_tkns] (parse-value tkns)]
      (cons item (lazy-seq (lazy-list remaining_tkns))))))

(defn objects-in [^java.io.Reader rdr]
  (lazy-list (drop-while #(not= % ["sym" \{]) (tokens (char-seq rdr)))))
