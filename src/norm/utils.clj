(ns norm.utils
  (:require [clojure.walk]))

(defn pmap-chunked [n f coll]
  (apply concat 
    (pmap #(doall (map f %)) (partition-all n coll))))

(defn pmapcat [f coll]
  (apply concat
    (pmap f coll)))

(defn pmapcat-chunked [n f coll]
  (apply concat
    (pmap-chunked n f coll)))

(defn unchunk [s]
  (when (first s)
    (lazy-seq
      (cons (first s) (unchunk (next s))))))

(defn pmapall [f coll]
  (let [num_threads (.. Runtime getRuntime availableProcessors)
        remaining   (atom ())
        func        (fn [item]
                      (let [result (f item)]
                        (swap! remaining next)
                        result))
        futures     (map #(future (func %)) (unchunk coll))]
    (reset! remaining (drop num_threads futures))
    (first @remaining) ; run the futures we just dropped + 1 more
    (map deref futures)))

(defn pmapall-chunked [n f coll]
  (apply concat
    (pmapall #(doall (map f %)) (partition-all n coll))))

(defn unique-id-getter
  "returns a function that assigns unique long ids to elements
   in an efficient, lockless, thread-safe manner.
   When passed no args, returns a map from elements to ids.
   When passed one arg, the element's id is created if it does not exist
   and then returned.
   When passed two args, if there is an id for the first it is returned,
   otherwise the value of the second arg is retured."
  ([] (unique-id-getter 0))
  ([start_value]
    (let [ids (atom {})]
      (fn 
        ([] @ids)
        ([k] (let [nids (swap! ids 
                          (fn [m]
                            (if (m k)
                              m
                              (assoc m k (+ start_value (count m))))))]
          (nids k)))
        ([k not_found]
          (@ids k not_found))))))

(defn counted-fn [f atom_num]
  (fn [& args]
    (let [ret (apply f args)]
      (swap! atom_num inc)
      ret)))

(defn counter 
  ([start_value]  (let [n (atom start_value)]
                    (fn ([i] (swap! n + i))
                        ([] @n))))
  ([] (counter 0)))

(defn map-counter
  ([start_value]  (let [m (atom {})]
                    (fn ([] @m)
                        ([k] (@m k))
                        ([k i] (swap! m update-in [k] #(+ i (or % start_value)))))))
  ([] (map-counter 0)))

(defn atomised-map-counter
  ([start_value]  (let [m (atom {})]
                    (fn ([] @m)
                        ([k] @(@m k))
                        ([k i] 
                          (if-let [a (@m k)]
                            (swap! a #(+ % i))
                            (swap! m assoc k (atom (+ i start_value))))))))
  ([] (atomised-map-counter 0)))

(defn update-with [f m] (into {} (for [[k v] m] [k (f v)])))

(defn indexify 
  ([coll]
    (indexify 0 coll))
  ([start coll]
    (map vector (iterate inc start) coll)))

(defmacro let-partial
  "macro to make assigning partial functions a little sexier.
  e.g.  (let-partial [(foo arg1 arg2)
                      (bar arg4)]
          (foo arg3)
          (bar))
  expands to
        (let [foo (partial foo arg1 arg2)
              bar (partial bar arg4)]
          (foo arg3)
          (bar))"
  [[& forms] & body]
  `(clojure.core/let
    ~(apply vector
      (mapcat
        (fn [[f & args :as things]]
          [f (cons partial things)])
        forms))
    (do ~@body)))

(defmacro pfor [bindings & body]
  (case (count (take 2 bindings))
    0 `(do ~@body)
    2 `(clojure.core/pmap
        (clojure.core/fn [~(first bindings)]
          (pfor [~@(drop 2 bindings)] ~@body))
        ~(second bindings))
    :else (throw (IllegalArgumentException. "pfor requries an even number of args."))))

(defmacro with-atoms [syms & body]
  `(clojure.core/let [~@(mapcat vector syms (repeat `(clojure.core/atom nil)))]
    ~@body))


(defn flat
  "like flatten but flattens maps too."
  [coll]
  (if (or (map? coll) (sequential? coll))
    (when-let [item (first coll)]
      (lazy-seq
        (if (or (map? item) (sequential? item))
          (concat (flat item) (flat (rest coll)))
          (cons item (flat (rest coll))))))
    coll))

(defmacro assign! [destructuring_expression value]
  (let [syms  (into {}
                (map vector
                  (filter symbol?
                    (flat destructuring_expression))
                  (repeatedly gensym)))
        de_ex (clojure.walk/postwalk-replace syms destructuring_expression)]
    `(clojure.core/let [~de_ex ~value] ~@(for [[x y] syms] `(clojure.core/reset! ~x ~y)))))


(defn map-merge [m1 m2]
  (if (and (map? m1) (map? m2))
    (merge-with map-merge m1 m2)
    m2))

