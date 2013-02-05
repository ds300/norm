(ns norm.io
  "This module handles file i/o."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as jio]
            [norm.json])
  (:import [cmu.arktweetnlp Twokenize]))

(def join-tokens #(apply str (interpose " " %)))

(defn- raw-seq
  "Returns a lazy seq of tweet objects in the given stream of
  newline-separated tweets"
  [^java.io.BufferedReader in]
   (if-let [text (.readLine in)]
     (cons
       {"text" text  "tokens" (into [] (Twokenize/tokenizeRawTweetText text))}
       (lazy-seq (raw-seq in)))
     (.close in)))

(defn- tkn-seq
  "Returns a lazy seq of tweet objects in the given token stream"
  [^java.io.BufferedReader in]
  (let [tokens (loop [acc (transient [])]
                 (let [line (.readLine in)]
                   (if (or (not line) (= "" line))
                     (persistent! acc)
                     (recur (conj! acc line)))))]
    (if (not= tokens [])
      (cons
        {"text" (join-tokens tokens) "tokens" tokens}
        (lazy-seq (tkn-seq in)))
      (.close in))))

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

