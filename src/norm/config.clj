(ns norm.config
  (:use [clojure.java.io :only (resource as-file)]
        [norm.utils :only (map-merge update-with)])
  (:gen-class))

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

; load default config
(def OPTS (atom (load-config (resource "config.edn"))))

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
      (swap! OPTS #(merge-with map-merge % hcfg)))))


; load the user's config file from cwd if it exists
(let [user_config_file (as-file "norm-config.edn")]
  (when (.isFile user_config_file)
    ; superimpose user config over default
    (swap! OPTS #(merge-with map-merge % (load-config user_config_file)))))


(defn opt
  "Get an option from the config map. ks are clojure keywords"
  [& ks]
  (or (get-in @OPTS ks) (throw (Exception. (str "no option at " ks)))))

(def coercers
  {
    clojure.lang.Keyword identity
    java.lang.String   identity
    java.lang.Long     #(Long. %)
    java.lang.Integer  #(Integer. %)
    java.lang.Double   #(Double. %)
    java.lang.Float    #(Float. %)
  })

(defn set-opt! [ks v]
  (if-let [orig (get-in @OPTS ks)]
    (swap! OPTS assoc-in ks ((coercers (type orig) read-string) v))
    (throw (Exception. (str "Trying to set invalid option: " ks)))))

(defn declare-opt! [& ks]
  (swap! OPTS assoc-in ks :undefined))

(defn def-opt! [[ks] v]
  (swap! OPTS assoc-in ks v))

(defn parse-opts [[a & [b & others :as more]]]
  (when a
    (if (= \: (first a))
      (let [loc (map (comp keyword last) (re-seq #":([a-z\-_]+)" a))]
        (set-opt! loc b)
        (lazy-seq (parse-opts others)))
      (cons a (lazy-seq (parse-opts more))))))

