(ns norm.core
  (:require [clojure.tools.cli :as cli]
            [clojure.java.io :as jio]
            [norm.trie :as trie]
            [norm.io :as io])
  ; (:gen-class)
  )

(defn print-help []
  (println "usage etc lol"))

(defn fail [message]
  (println message)
  (print-help)
  (System/exit 1))

; i'll change this guff to proper reporting functions 
(def DATA_FILES [
  ["/nmd.txt" (fn [filename] (io/doing-done "Reading nmd.txt" (into {} (io/parse-tsv filename))))]
  ["/dict.txt" (fn [filename] (io/doing-done "Reading dict.txt" (trie/trie (map #(conj % 0) (io/parse-tsv filename)))))]
])

(defn process-batch-args [input_file_name & args]
  (let [[opts _ _] (cli/cli args
    ["-i" "--input-format" "(json|raw|tkn)" :default "raw"]
    ["-o" "--output-format" "(json|raw|tkn) defaults to whichever input format is used"]
    ["-d" "--data-dir" "the data directory to use"]
    ["-f" "--output-file" "the path of the output file. <input-path>.norm is used by default"])
        output_format (:output-format opts (:input-format opts))
        data_dir      (java.io.File. (:data-dir opts "./data"))
        input_stream  (io/prog-reader input_file_name)
        output_stream (jio/writer (:output-file opts (str input_file_name ".norm")))]
    (when-not (every? #{"json" "raw" "tkn"} [(:input-format opts) output_format])
      (fail "invalid file format(s)"))
    (if-not (.isDirectory data_dir)
      (fail "data directory does not exist")
      (let [resources (map (fn [[p f]] (f (str (.getAbsolutePath data_dir) p))) DATA_FILES)]
        [input_stream, output_stream, (:input-format opts), output_format, resources]))))



(def commands {
  "batch"
  (fn [args]
    (let [[in, out, inf, outf, [NMD DICT]] (apply process-batch-args args)
          encode (io/encoders outf)]
      (defn normalise-token-list [tokens]
        (for [t tokens] (if (DICT t) t (if-let [r (NMD t)] r t))))

      (defn normalise-tweet [tweet]
        (assoc tweet "norm_tokens" (normalise-token-list (tweet "tokens"))))

      (when (= outf "json") (.write out "["))
      (dorun (map #(.write out %) (pmap encode (pmap normalise-tweet (io/get-stream inf in)))))
      (when (= outf "json") (.write out "]"))
      (shutdown-agents)
      (.close out)))
})

(defn -main
  "I don't do a whole lot."
  [command & args]
  (if-let [command-fn (commands command)]
    (command-fn args)
    (fail (str "Unrecognised command: " command))))
