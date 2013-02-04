(ns norm.io
  "This module handles file i/o."
  (:require [clojure.data.json :as json])
  (:import [cmu.arktweetnlp Twokenize]))


(defn raw-seq
  "Returns a lazy seq of tweet objects in the given stream of
  newline-separated tweets"
  [^java.io.BufferedReader in]
  (if-let [text (.readLine in)]
    (cons
      {"text" text  "tokens" (Twokenize/tokenize text)}
      (lazy-seq (raw-seq in)))
    (.close in)))

(defn tkn-seq
  "Returns a lazy seq of tweet objects in the given token stream"
  [^java.io.BufferedReader in]
  (let [tokens (loop [acc (transient [])]
                 (let [line (.readLine in)]
                   (if (or (not line) (= "" line))
                     (persistent! acc)
                     (recur (conj! acc line)))))]
    (if (not= tokens [])
      (cons
        {"text" (apply str (interpose " " tokens)) "tokens" tokens}
        (lazy-seq (tkn-seq in)))
      (.close in))))

(defn json-seq "Returns a lazy seq of tweet objects in the given token stream"
  [^java.io.BufferedReader in]
  (when-let [obj (json/read in)]
    (when-not (obj "text")
      (do
        (.close in)
        (throw (Exception. "Bad JSON object. No 'text' key"))) )
    (cons
      (conj obj ["tokens" (Twokenize/tokenize (obj "text"))])
      (lazy-seq (json-seq in)))))

