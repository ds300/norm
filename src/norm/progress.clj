(ns norm.progress)

(def LAST_STRING_LENGTH (atom 0))
(def ^:dynamic GET (fn [] ""))
(def ALIVE (atom false))
(def ^:dynamic SLEEP_INTERVAL 200)
(def WORKER_THREAD (atom nil))

(defn- spaces [n] (apply str (repeat n " ")))

(defn- daemon []
  (Thread/sleep SLEEP_INTERVAL)
  (print "\r")
  (let [newstring (GET)]
    (print (str newstring (spaces (- @LAST_STRING_LENGTH (count newstring)))))
    (.flush *out*)
    (reset! LAST_STRING_LENGTH (count newstring)))
  (if @ALIVE
    (recur)
    (do (println) (.flush *out*))))

(defn start! []
  (reset! ALIVE true)
  (reset! WORKER_THREAD (future-call daemon)))

(defn stop! []
  (reset! ALIVE false)
  @@WORKER_THREAD)

(defmacro monitor
  [args & body]
  `(binding [GET ~(first args)
             SLEEP_INTERVAL ~(or (second args) SLEEP_INTERVAL)]
    (start!)
    (let [result# (do ~@body)]
      (stop!)
      result#)))
