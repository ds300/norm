(ns norm.io
  "This module handles file i/o."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as jio]
            [norm.json])
  (:import [cmu.arktweetnlp Twokenize]))

(def join-tokens #(apply str (interpose " " %)))

(defn line-seq-with-close
  "Returns the lines of text from rdr as a lazy sequence of strings.
  Closes rdr when done.
  rdr must implement java.io.BufferedReader."
  [^java.io.BufferedReader rdr]
  (if-let [line (.readLine rdr)]
    (cons line (lazy-seq (line-seq-with-close rdr)))
    (.close rdr)))

(defn- consume-raw [line]
  {"text" line "tokens" (into [] (Twokenize/tokenizeRawTweetText line))})

(defn- raw-seq [^java.io.Reader in]
  (map consume-raw (line-seq-with-close in)))

(defn- group-tkns [lines]
  (filter #(not= '("") %) (partition-by empty? lines)))

(defn- consume-tkn [tokens]
  {"text" (join-tokens tokens) "tokens" tokens})

(defn- tkn-seq [in]
  (map consume-tkn (group-tkns (line-seq-with-close in))))

(defn- consume-json [obj]
  (if (obj "tokens")
    ; if the tokens have been included, just use them
    (if-not (obj "text")
      (conj obj ["text" (join-tokens (obj "tokens"))]) ; add original text if not present
      obj)
    (if-not (obj "text")
      ; if we can't find text or tokens, it is an error. throw exception.
      (throw (Exception. (str "Bad JSON object. No 'text' or 'tokens' field:\n" obj)))
      (conj obj ["tokens" (into [] (Twokenize/tokenizeRawTweetText (obj "text")))]))))

(defn- json-seq
  "Returns a lazy seq of tweet objects in the given stream"
  [^java.io.BufferedReader in]
  (map consume-json (norm.json/objects-in in)))

(defn get-stream [format filename]
  (let [rdr (jio/reader filename)]
    (({"raw" raw-seq, "tkn" tkn-seq, "json" json-seq} format) rdr)))

