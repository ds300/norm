(ns norm.config
  (:use [clojure.java.io :only (resource as-file)]))

; execution directory
(def EX_DIR (.getParent (-> "" resource as-file)))

(defn bracktise [s]
  (str "{" s "}"))

(defn load-config [from]
  (-> from slurp bracktise read-string eval))

(def ^:dynamic OPTS (atom (load-config (resource "config.edn"))))

(defn- map-merge [m1 m2]
  (if (and (map? m1) (map? m2))
    (merge-with map-merge m1 m2)
    m2))

(let [user_config_file (as-file (str EX_DIR "/config.edn"))]
  (when (.isFile user_config_file)
    (swap! OPTS #(merge-with map-merge % (load-config user_config_file)))))

