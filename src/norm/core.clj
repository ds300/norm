(ns norm.core
  (:require [norm.config :as config]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as jio]
            [norm.trie :as trie]
            [norm.clean :as clean]
            [norm.data :as data]
            [norm.io :as io]
            [norm.train.nmd]
            [norm.train.twt-c]
            [norm.train.lksm]
            [norm.train.dpb]
            [norm.train.dm-dict]
            [norm.train.tlm])
  (:gen-class))

(def ARGS (atom nil))




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
    (fn [args]
      (let [[id & [outpath & extra]] args]
        (cond
          extra
            (fail (str "unrecognised args: " extra))
          (not id)
            (fail "No training file id given.")
          (not (#{"dm-dict" "twt-c" "nmd" "dpb" "tlm" "lksm"} id))
            (fail (str "invalid training file: " id))
          :else ;bind the global output path and train
            (binding [io/OUT_PATH (or outpath (data/get-path (keyword id)))]
              ((eval (symbol (str  "norm.train."id "/train!"))))))))

  "bootstrap"
    (fn [args]
      (if (seq args)
        (fail (str "unrecognised args: " args))
        (do
          (data/verify-readable! :twt :dict :nyt)
          (data/verify-writeable! :twt-c :dm-dict :nmd :tlm :dpb :lksm)
          ((commands "train") ["twt-c"])
          ((commands "train") ["dm-dict"])
          ((commands "train") ["nmd"])
          ((commands "train") ["tlm"])
          ((commands "train") ["dpb"])
          ((commands "train") ["lksm"]))))

  "clean"
    (fn [args]
      (if (< (count args) 3)
        (fail "clean requires input tweets,
               output destination, and at least
               one filter operation")
        (apply clean/clean args)))
})



(defn -main
  "I don't do a whole lot."
  [& args]

  ; get paths from command line
  (let [cmd_args (config/parse-opts args)]
   

    ; now decide which command to dispatch to
    (if-let [command-fn (commands (first cmd_args))]
      (command-fn (rest cmd_args))
      (fail (str "Unrecognised command: " (first @ARGS)))))


  (shutdown-agents))


