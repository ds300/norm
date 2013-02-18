(ns norm.io
  "This module handles file i/o."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as jio]
            [norm.json])
  (:import [cmu.arktweetnlp Twokenize])
  (:use [clojure.string :only (join)]))

(def ^:dynamic IN)
(def ^:dynamic OUT_PATH)

(def join-tokens #(apply str (interpose " " %)))

(defn prog-reader [filename]
  (norm.jvm.ProgressTrackingBufferedFileReader/make filename))

(defn prog-reader-gz [filename]
  (norm.jvm.ProgressTrackingBufferedFileReader/makeGzip filename))

(defmacro doing-done [msg & body]
  `(do 
    (let [result# (do (print ~msg "... ")
    (.flush *out*) ~@body)]
      (println "done!")
      result#)))

(defn writer-gz [filename]
  (-> filename
    (java.io.FileOutputStream.)
    (java.util.zip.GZIPOutputStream.)
    (java.io.OutputStreamWriter.)))


(defn line-seq-with-close
  "Returns the lines of text from rdr as a lazy sequence of strings.
  Closes rdr when done.
  rdr must implement java.io.BufferedReader."
  [^java.io.BufferedReader rdr]
  (if-let [line (.readLine rdr)]
    (cons line (lazy-seq (line-seq-with-close rdr)))
    (.close rdr)))

(defn lines-in [filename]
  (line-seq-with-close (jio/reader filename)))

(defn by-lines
  "Returns a lazy seq of func applied to the lines in the specified file,
  ignoring blank lines"
  [filename func]
  (map func (filter not-empty (lines-in filename))))


(defn parse-tsv [filename & funcs]
  (by-lines filename
    (fn [line]
      (mapv
        (fn [f v] (f v))
        (concat funcs (repeat identity))
        (clojure.string/split line #"\t")))))

(defn spit-tsv [out vecs]
  (doseq [vec vecs]
    (.write out (apply str (interpose "\t" vec)))
    (.write out "\n")))

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

(defn get-stream [format in]
  (({"raw" raw-seq, "tkn" tkn-seq, "json" json-seq} format) in))

(def encoders
  {"raw" (fn [tweet]
           (str (join " " (tweet "norm_tokens")) "\n"))
   "tkn" (fn [tweet]
           (str (join "\n" (tweet "norm_tokens")) "\n"))
   "json" (fn [tweet]
            (str (json/write-str tweet) ",\n"))
  }
)

(defn open- [mode f]
  (let [path (if (instance? java.io.File f) (.getAbsolutePath f) f)]
    (case mode
      :w  (clojure.java.io/writer path)
      :wz (writer-gz path)
      :r  (prog-reader path)
      :rz (prog-reader-gz path))))



(defmacro open
  [bindings & body]
  (cond
    (= 0 (count bindings))
      `(do ~@body)
    (= 0 (mod (count bindings) 3))
      (let [[mode id f & others] bindings]
        `(clojure.core/let [~id (norm.io/open- ~mode ~f)]
           (try
             (norm.io/open ~(drop 3 bindings) ~@body)
             (finally ~@(filter identity
                          [(when (#{:w :wz} mode) `(.flush ~id)) `(.close ~id)])))))
  
    :else (throw (IllegalArgumentException.
                   (str "number of forms in binding vector must be a multiple of three" bindings)))))

