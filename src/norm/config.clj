(ns norm.config
  (:use [clojure.java.io :only (resource as-file)]
        [norm.utils :only (map-merge update-with)]
        [cfg.core]))

(init)

(defopt :help
  :bool true
  :aliases ["h" "-help"]
  :help-string "Print all available options")

(defopts :batch
  (defopt :input-format
    :default     "raw"
    :validate    #{"raw" "json" "tkn"}
    :aliases     ["i" "-input-format"]
    :help-string "Sets the input format of the file to be used for batch processing. Must be one of (raw|tkn|json). Default is raw.")
  (defopt :output-format
    :default     "tkn"
    :validate    #{"raw" "json" "tkn"}
    :aliases     ["o" "-output-format"]
    :help-string "Sets the output type for batch processing. Must be one of (raw|tkn|json). Default is tkn.")
  (defopt :normaliser-type
    :default     "simple"
    :validate    #{"simple" "complex" "duplex"}
    :aliases     ["t" "-normaliser-type"]
    :help-string "Sets the type of normalisation to perform. Must be one of (simple|complex|duplex). Default is simple."))

(defopts :data
  (defopt :dir
    :default     "./data"
    :validate    #(.isDirectory (as-file %))
    :aliases     ["d" "-data-dir"]
    :help-string "Sets the directory in which data files are assumed to be if not otherwise specified.")
  (defopts :paths
    (defopt :nmd
      :aliases     ["-nmd"]
      :help-string "Sets the path to the normalisation dictionary.")
    (defopt :tlm
      :aliases     ["-tlm"]
      :help-string "Sets the path to the trigram language model.")
    (defopt :dict
      :aliases     ["-dict"]
      :help-string "Sets the path to the in-vocabulary dictionary.")
    (defopt :dm-dict
      :aliases     ["-dm-dict"]
      :help-string "Sets the path to the double-metaphone dictionary.")
    (defopt :lksm
      :aliases     ["-lksm"]
      :help-string "Sets the path to the liblinear model.")
    (defopt :profiles
      :aliases     ["-profiles"]
      :help-string "Sets the path to the langdetect profiles.")
    (defopt :twt
      :aliases     ["-twt"]
      :help-string "Sets the path to the noisy tweet corpus file.")
    (defopt :twt-c
      :aliases     ["-twt-c"]
      :help-string "Sets the path to the clean tweet corpus file.")
    (defopt :dpb
      :aliases     ["-dpb"]
      :help-string "Sets the path to the dependency bank.")
    (defopt :nyt
      :aliases     ["-nyt"]
      :help-string "Sets the path to the gigaword folder containin gzipped NYT xml data.")))

(def atoi #(Integer. %))
(def atod #(Double. %))
(def nat? #(or (pos? %) (zero? %)))

(defopts :confusion-sets
  (defopt :lex-dist
    :default     2
    :parse       atoi
    :validate    nat?
    :aliases     ["ld" "-lex-dist"]
    :help-string "Sets the maximum lexical edit distance for words which appear in confusion sets. Default is 2")
  (defopt :phon-dist
    :default     1
    :parse       atoi
    :validate    nat?
    :aliases     ["pd" "-phon-dist"]
    :help-string "Sets the maximum phonemic edit distance for words which appear in confusion sets. Default is 1")
  (defopt :post-rank-cutoff
    :default     10
    :parse       atoi
    :validate    pos?
    :aliases     ["prc" "-post-rank-cutoff"]
    :help-string "The percentage cutoff to apply to confusion sets post-ranking. Default is 10."))

(defopt :buffer-size
  :default     8192
  :parse       atoi
  :validate    pos?
  :aliases     ["b" "-buffer-size"]
  :help-string "The size (in bytes) of buffers to use in buffered readers or writers. Default is 8192.")

(defopts :dict
  (defopt :include
    :default #{"retweet" "followback" "lol" "lmao" "haha" "rofl"}
    :parse   #(clojure.string/split % ",")
    :merge   into)
  (defopt :exclude
    :default #{}
    :parse   #(clojure.string/split % ",")
    :merge   into))

(defopts :train
  (defopts :nmd
    (defopt :post-rank-cutoff
      :default  30
      :parse    atoi
      :validate pos?
      :aliases  ["nprc" "-nmd-post-rank-cutoff"]
      :help-string
      "Sets the percentage cutoff for confusion sets when creating NMD. Default is 30.")
    (defopt :min-freq
      :default  10
      :parse    atoi
      :validate nat?
      :aliases  ["mf" "-nmd-min-freq"]
      :help-string
      "Sets the minimum frequency an OOV word must have before it is considered for inclusion in NMD. Default is 10.")
    (defopt :min-length
      :default  4
      :parse    atoi
      :validate pos?
      :aliases  ["ml" "-nmd-min-length"]
      :help-string
      "Sets the minimum length an OOV word must have before it is considered for inclusion in DNM. Default is 4.")
    (defopt :pair-rank-cutoff
      :default  40000
      :parse    atoi
      :validate pos?
      :aliases  ["np" "-nmd-num-pairs"]
      :help-string "The number of pairs to include in the final NMD. Default 40000.")
    (defopt :n-gram-order
      :default  2
      :parse    atoi
      :validate pos?
      :aliases  ["nn" "-nmd-n-gram-order"]
      :help-string
      "Sets the n-gram order for contextual features when creating NMD. Default is 2.")
    (defopt :window-size
      :default  3
      :parse    atoi
      :validate pos?
      :aliases  ["nw" "-nmd-window-size"]
      :help-string
      "Sets the number of n-grams to take from either side of the target token when extracting contextual featurs. Default 3.")
    (defopt :measure
      :default "Lin"
      :validate #{"Confusion"
                  "Cosine"
                  "CosineMi"
                  "CrMi"
                  "Dice"
                  "DiceMi"
                  "Hindle"
                  "Jaccard"
                  "JaccardMi"
                  "Jensen"
                  "Lee"
                  "Lin"
                  "Lp"
                  "Overlap"
                  "RecallMi"
                  "Tanimoto"}
      :aliases ["nm" "-nmd-similarity-measure"]
      :help-string
      "Specifies the distributional similarity measure to use for generating NMD.
      See uk.ac.susx.mlcl.byblo.measures for a list of available measures.
      Default is Lin."))

  (defopts :dpb
    (defopt :num-sents
      :default 40000000
      :parse atoi
      :validate nat?
      :aliases ["dn" "-dpb-num-sents"]
      :help-string "The number of sentences to use when extracting features from NYT."))

  (defopts :lksm
    (defopt :chunksize
      :default 1
      :parse atoi)
    (defopt :num-tweets
      :default 10000000
      :parse atoi
      :validate nat?
      :aliases ["ln" "-lksm-num-tweets"]
      :help-string "The number of tweets to use when constructing the liblinear model. Default is 10000000")
    (defopt :bias
      :default 1.0
      :parse atod
      :aliases ["lb" "-lksm-bias"]
      :help-string "The bias to use when training the liblinear model. Default is 1.0")
    (defopt :eps
      :default 0.01
      :parse atod
      :aliases ["le" "-lksm-eps"]
      :help-string "The value for eps to pass into the liblinear training process. Default is 0.01")
    (defopt :c
      :default 1.0
      :parse atod
      :aliases ["lc" "-lksm-c"]
      :help-string "The value for c to pass into the liblinear training process. Default is 1.0")
    (defopt :solver
      :default "L2R_LR"
      :validate #{"L2R_LR"
                  "L2R_L2LOSS_SVC_DUAL"
                  "L2R_L2LOSS_SVC"
                  "L2R_L1LOSS_SVC_DUAL"
                  "MCSVM_CS"
                  "L1R_L2LOSS_SVC"
                  "L1R_LR"}
      :aliases ["ls" "-lksm-solver"]
      :help-string "The solver type to use for the libelinear model. Default is L2R_LR")
    (defopt :num-candidates
      :default 1)))

                  
; config files stored as implicit map, so need to put {}s around it after slurping
(defn bracktise
  "converts s to {s\\n}"
  [s]
  (str "{" s "\n}"))

(defn load-config
  "reads a config map"
  [from]
  (-> from slurp bracktise read-string))

(defn ensure-absolute
  "Returns pathname if absolute, or resolves it's absolute path
  with respect to rel_dir and returns that."
  [rel_dir pathname]
  (let [f (as-file pathname)]
    (if (.isAbsolute f)
      pathname
      (.getAbsolutePath (java.io.File. rel_dir pathname)))))


; load the user's config file from home dir if it exists
(let [home_dir (System/getProperty "user.home")
      home_config_file (as-file (str home_dir "/.norm-config.edn"))]
  (when (.isFile home_config_file)
    (let [hcfg (atom (load-config home_config_file))]
      (when-let [datadir (get-in hcfg [:data :dir])]
        (swap! hcfg assoc-in [:data :dir] (ensure-absolute home_dir datadir)))
      (when-let [paths (get-in hcfg [:data :paths])]
        (swap! hcfg assoc-in [:data :paths]
          (update-with (partial ensure-absolute home_dir) paths)))
      (merge-public-opts! hcfg))))


; load the user's config file from cwd if it exists
(let [user_config_file (as-file "norm-config.edn")]
  (when (.isFile user_config_file)
    ; superimpose user config over default
    (merge-public-opts! (load-config user_config_file))))



