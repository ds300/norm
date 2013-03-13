(ns norm.core
  (:require [norm.config :as config]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as jio]
            [norm.trie :as trie]
            [norm.alise :as alise]
            [norm.clean :as clean]
            [norm.progress :as progress]
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

(def normaliser-fns {
  "simple" norm.alise/get-simple-normaliser-fn
  "complex" norm.alise/get-complex-normaliser-fn
  "duplex" norm.alise/get-duplex-normaliser-fn
})

(def commands {
  "batch"
  (fn [args]
    (let [[in_path out_path & others] args
          inf (config/opt :batch :input-format)
          outf (config/opt :batch :output-format)
          encode (io/encoders outf)
          normalise-token-list ((normaliser-fns (config/opt :batch :normaliser-type)))
          normalise-tweet (fn [tweet]
                            (assoc tweet "norm_tokens" (normalise-token-list (tweet "tokens"))))]

      (io/open [:r in in_path
                :w out (or out_path (str in_path ".norm"))]
        (when (= outf "json") (.write out "["))
        (progress/monitor [#(.progress in) 500]
          (dorun 
            (map (comp #(.write out %) encode)
              (pmap normalise-tweet (io/get-stream inf in)))))
        (when (= outf "json") (.write out "]")))))
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


