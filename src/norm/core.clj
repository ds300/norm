(ns norm.core
  (:require [clojure.tools.cli :as cli]
            [clojure.java.io :as jio]
            [norm.trie :as trie]
            [norm.data :as data]
            [norm.io :as io]
            [norm.train.dm-dict :as dm-dict])
  ; (:gen-class)
  )

(defn print-help []
  (println "usage etc lol"))

(defn fail [message]
  (println message)
  (print-help)
  (System/exit 1))


; (defn process-batch-args [input_file_name & args]
;   (let [[opts _ _] (cli/cli args
;     ["-i" "--input-format" "(json|raw|tkn)" :default "raw"]
;     ["-o" "--output-format" "(json|raw|tkn) defaults to whichever input format is used"]
;     ["-f" "--output-file" "the path of the output file. <input-path>.norm is used by default"])
;         output_format (:output-format opts (:input-format opts))
;         data_dir      (java.io.File. (:data-dir opts "./data"))
;         input_stream  (io/prog-reader input_file_name)
;         output_stream (jio/writer (:output-file opts (str input_file_name ".norm")))]
;     (when-not (every? #{"json" "raw" "tkn"} [(:input-format opts) output_format])
;       (fail "invalid file format(s)"))
;     (if-not (.isDirectory data_dir)
;       (fail "data directory does not exist")
;       (let [resources (map (fn [[p f]] (f (str (.getAbsolutePath data_dir) p))) DATA_FILES)]
;         [input_stream, output_stream, (:input-format opts), output_format, resources]))))



(defn set-data-paths!
  [args]
  "returns unused args"
  (let [[opts unused _] (apply cli/cli
                          (into
                            [args ["-d" "--data-dir" "the data directory to use" :default "./data"]]
                            (map
                              #(do [(str "--" %) (str "override " % " path")])
                              (map name data/FILES))))]
    (data/set-paths! (:data-dir opts))
    (doseq [[k v] (dissoc opts :data-dir)]
      (data/set-path! k v))
    unused))


(def commands {
  ; "batch"
  ; (fn [args]
  ;   (let [[in, out, inf, outf, [NMD DICT]] (apply process-batch-args args)
  ;         encode (io/encoders outf)]
  ;     (defn normalise-token-list [tokens]
  ;       (for [t tokens] (if (DICT t) t (if-let [r (NMD t)] r t))))

  ;     (defn normalise-tweet [tweet]
  ;       (assoc tweet "norm_tokens" (normalise-token-list (tweet "tokens"))))

  ;     (when (= outf "json") (.write out "["))
  ;     (dorun (map #(.write out %) (pmap encode (pmap normalise-tweet (io/get-stream inf in)))))
  ;     (when (= outf "json") (.write out "]"))
  ;     (shutdown-agents)
  ;     (.close out)))
  "train"
  (fn [id outfile & args]
    (with-open [out (jio/writer (if (= "overwrite" outfile) (@data/PATHS (keyword id)) outfile))]
      (binding [io/OUT out]
        (case (keyword id)
          :dm-dict (data/load-and-bind [:dict]
                     (dm-dict/train))
          (fail (str "invalid training file: " id))))))
})

(defn -main
  "I don't do a whole lot."
  [& args]
  ; start off by setting up the data paths and discarding any
  ; associated args
  (let [[command & args-] (set-data-paths! args)]
    ; now decide which command to dispatch to
    (if-let [command-fn (commands command)]
      (apply command-fn args-)
      (fail (str "Unrecognised command: " command)))))
