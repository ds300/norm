(ns norm.core
  (:require [clojure.tools.cli :as cli])
  (:gen-class))

(defn print-help []
  (println "usage etc lol"))

(def commands {
  "batch"
  (fn [input_file_name & args]
    (let [[opts _ _] (cli/cli args
    ["-i" "--input-format" "(json|raw|tkn)" :default "tkn"]
    ["-o" "--output-format" "(json|raw|tkn) defaults to whichever input format is used"]
    ["-d" "--data-dir" "the data directory to use"])]))
})

(defn -main
  "I don't do a whole lot."
  [command & args]
  (if-let [command-fn (commands command)]
    (command-fn args)
    (print-help)))
