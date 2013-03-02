(ns norm.io
  "This module handles file i/o."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as jio]
            [norm.config :as config]
            [norm.json])
  (:import [cmu.arktweetnlp Twokenize])
  (:use [clojure.string :only (join)]))

(def ^:dynamic IN)
(def ^:dynamic OUT_PATH)

(def join-tokens #(apply str (interpose " " %)))

(defmacro doing-done
  "Prints msg and then \"... \", executes body,
  then prints \"done!\""
  [msg & body]
  `(do 
    (let [result# (do (print ~msg "... ")
    (.flush *out*) ~@body)]
      (println "done!")
      result#)))

(defn reader
  "Progress reporting file reader. call .progress for progress string"
  ([filename buffer_size]
    (norm.jvm.ProgressReportingBufferedFileReader/make filename buffer_size))
  ([filename]
    (reader filename (config/opt :buffer-size))))

(defn reader-gz
  "Progress reporting file reader. call .progress for progress string"
  ([filename buffer_size]
    (norm.jvm.ProgressReportingBufferedFileReader/makeGzip filename buffer_size))
  ([filename]
    (reader-gz filename (config/opt :buffer-size))))

(defn writer-gz
  "Creates a gzip file writer for the given path"
  ([filename buffer_size]
    (-> filename
      (java.io.FileOutputStream.)
      (java.util.zip.GZIPOutputStream.)
      (java.io.OutputStreamWriter.)
      (java.io.BufferedWriter. buffer_size)))
  ([filename]
    (writer-gz filename (config/opt :buffer-size))))

(defn writer
  ([filename buffer_size]
    (-> filename
      (java.io.FileWriter.)
      (java.io.BufferedWriter. buffer_size)))
  ([filename]
    (writer filename (config/opt :buffer-size))))


(defn line-seq-with-close
  "Returns the lines of text from rdr as a lazy sequence of strings.
  Closes rdr when done.
  rdr must implement java.io.BufferedReader."
  [^java.io.BufferedReader rdr]
  (if-let [line (.readLine rdr)]
    (cons line (lazy-seq (line-seq-with-close rdr)))
    (.close rdr)))

(defn lines-in
  "Returns a lazy seq of the lines in the specified file.
  closes the file when all lines have been read."
  [filename]
  (line-seq-with-close (jio/reader filename)))

(defn by-lines
  "Returns a lazy seq of func applied to the lines in the specified file,
  ignoring blank lines. closes the file when all lines have been read."
  [filename func]
  (map func (filter not-empty (lines-in filename))))


(defn parse-tsv
  "takes lines from filename, splits them on \\t,
  then applies funcs to them. i.e. with file:
       hello\t3
  calling (parse-tsv filename identity #(Integer. ))
  yields ([\"hello\" 3])"
  [filename & funcs]
  (by-lines filename
    (fn [line]
      (mapv
        (fn [f v] (f v))
        (concat funcs (repeat identity))
        (clojure.string/split line #"\t")))))

(defn spit-tsv
  "Takes a writer and a collection of seqs, and prints
  the contents of those seqs separated by tabs on separate lines."
  [out vecs]
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

(defn get-stream
  "Returns a lazy seq of tweet objects from the given input stream
  in the given format."
  [format in]
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

(defn open-
  "Returns a reader or writer, depending on the specified mode. f can
  be either a path string or a java.io.File object. 
  Valid modes are:
    :w (buffered writer)
    :wz (gzip writer)
    :r (progress tracking buffered reader)
    :rz (progress tracking gzip reader)"
  [mode f buffer_size]
  (let [path (if (instance? java.io.File f) (.getAbsolutePath f) f)
        func (case mode
              :w  writer
              :wz writer-gz
              :r  reader
              :rz reader-gz
              (throw (Exception. (str "Invalid write mode " mode))))]
    (func path buffer_size)))

(defmacro open
  "binds readers or writers to names. bindings are triples
  of the form mode, name, file/path.
  e.g. (open [:r in some_path]) 
  is equal to (let [in (open- :r some_path)])
  Its much less verbose when you're opening multiple files."
  [bindings & body]
  (if (= 0 (count bindings))
   `(do ~@body)
    (let [[mode id f & [buf bufsz & more :as others]] bindings
           do_more (= buf :buf)
           buffer_size (if do_more bufsz `(config/opt :buffer-size))]
      `(clojure.core/let [~id (norm.io/open- ~mode ~f ~buffer_size)]
         (try
           (norm.io/open [~@(if do_more more others)] ~@body)
           (finally ~@(filter identity
                        [(when (#{:w :wz} mode) `(.flush ~id)) `(.close ~id)])))))))
  
