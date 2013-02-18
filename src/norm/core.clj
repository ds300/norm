(ns norm.core
  (:require [norm.config :as config]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as jio]
            [norm.trie :as trie]
            [norm.data :as data]
            [norm.io :as io]
            [norm.train.dm-dict :as dm-dict]
            [norm.train.twt-c :as twt-c]
            [norm.train.nmd :as nmd]
            [norm.train.tlm :as tlm]
            [norm.train.dpb :as dpb]
            [norm.train.lksm :as lksm])
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
  "train" (fn []
    (let [[id outfile & other_args] @ARGS]
      (binding [io/OUT_PATH (if (= ":o" outfile) (data/get-path (keyword id)) outfile)]
        (case (keyword id)
          :dm-dict (data/load-and-bind [:dict]
                     (dm-dict/train))
          :twt-c (data/load-and-bind [:dict]
                     (twt-c/train))
          :nmd (data/load-and-bind [:dict :dm-dict]
                 (nmd/train))
          :dpb (dpb/train)
          :tlm (tlm/train)
          :lksm (lksm/train)
          (fail (str "invalid training file: " id))))))
})




(defn -main
  "I don't do a whole lot."
  [& args]
  (reset! ARGS args)

  ;; start off by setting up the manually-specified data paths
  ; get paths from command line
  (let [[opts unused _] (apply cli/cli
                          (into
                            [@ARGS ["-d" "--data-dir" "the data directory to use"]]
                            (map
                              #(do [(str "--" %) (str "override " % " path")])
                              (map name data/FILES))))]

    ; swap dir if necessary
    (when-let [data-dir (:data-dir args)]
      (swap! config/OPTS assoc-in [:data :dir] data-dir))
    
    ; swap all other specified paths
    (doseq [[k v] (dissoc opts :data-dir)]
      (data/set-path! k v))

    ; remove any consumed args
    (reset! ARGS unused))

    ; now decide which command to dispatch to
    (if-let [command-fn (commands (first @ARGS))]
      (do (swap! ARGS rest) (command-fn))
      (fail (str "Unrecognised command: " (first @ARGS))))


  (shutdown-agents))


