(ns norm.utils
  "Miscellaneous utilities up in here."
  (:require [clojure.walk]))

(defn pmap-chunked
  "maps f over coll in chunks of size n, to reduce concurrency overhead"
  [n f coll]
  (apply concat 
    (pmap #(doall (map f %)) (partition-all n coll))))

(defn pmapcat
  "Like mapcat, but with pmap."
  [f coll]
  (apply concat
    (pmap f coll)))

(defn pmapcat-chunked
  "like pmap-chunked, but with pmapcat."
  [n f coll]
  (apply concat
    (pmap-chunked n f coll)))

(defn unchunk
  "Takes a chunked-seq and returns a lazy-seq of its items."
  [s]
  (when (first s)
    (lazy-seq
      (cons (first s) (unchunk (next s))))))

(defn at-least
  "returns true iff n elements in coll satisfy pred"
  [n pred [x & xs :as coll]]
  (cond
    (<= n 0)         true
    (not (seq coll)) false
    (pred x)         (recur (dec n) pred xs)
    :otherwise       (recur n pred xs)))

(defn pmapall
  "like pmap, but eager, and with more efficient core usage."
  [f coll]
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

(defn pmapall-chunked
  "like pmap-chunked, but with pmapall"
  [n f coll]
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

(defn counted-fn
  "takes a function and an atomic number, returning a function which
  returns f applied to its args while incrementing the atomic number"
  [f atom_num]
  (fn [& args]
    (let [ret (apply f args)]
      (swap! atom_num inc)
      ret)))

(defn counter 
  "returns a function which, when called with an integer
  argument i, increments an internal counter by i. When called with
  no arguments, returns the current value of the counter."
  ([start_value]  (let [n (atom start_value)]
                    (fn ([i] (swap! n + i))
                        ([] @n))))
  ([] (counter 0)))

(defn map-counter
  "returns a function which, when called with anything and an integer
  i, increments a counter for the anything in a map by i. When called
  with only an anything, returns the value for the counter associated
  with anything. When called with no arguments, returns the underlying
  map."
  ([start_value]  (let [m (atom {})]
                    (fn ([] @m)
                        ([k] (@m k))
                        ([k i] (swap! m update-in [k] #(+ i (or % start_value)))))))
  ([] (map-counter 0)))

(defn atomised-map-counter
  "as map-counter, but the numbers are stored in individual atoms."
  ([start_value]  (let [m (atom {})]
                    (fn ([] @m)
                        ([k] (when-let [a (@m k)] @a))
                        ([k i] 
                          (if-let [a (@m k)]
                            (swap! a #(+ % i))
                            (swap! m assoc k (atom (+ i start_value))))))))
  ([] (atomised-map-counter 0)))

(defn update-with
  "applies f to all the vals in m, returning a new map."
  [f m] (into {} (for [[k v] m] [k (f v)])))

(defn take-percent
  "takes p percent of coll"
  [p coll]
  (let [n (count coll)]
    (take (int (-> n (/ 100) (* p))) coll)))

(defn indexify
  "returns tuple vectors where the first element is an index
  and the second element is the corresponding element in coll."
  ([coll]
    (indexify 0 coll))
  ([start coll]
    (map vector (iterate inc start) coll)))

(defmacro with-atoms
  "Binds one or more local names to empty atoms."
  [syms & body]
  `(clojure.core/let [~@(mapcat vector syms (repeat `(clojure.core/atom nil)))]
    ~@body))


(defn flat
  "like clojure.core/flatten but flattens maps too."
  [coll]
  (if (or (map? coll) (sequential? coll))
    (when-let [item (first coll)]
      (lazy-seq
        (if (or (map? item) (sequential? item))
          (concat (flat item) (flat (rest coll)))
          (cons item (flat (rest coll))))))
    coll))

(defmacro assign!
  "reset!s atoms with destructuring."
  [destructuring_expression value]
  (let [syms  (into {}
                (map vector
                  (filter symbol?
                    (flat destructuring_expression))
                  (repeatedly gensym)))
        de_ex (clojure.walk/postwalk-replace syms destructuring_expression)]
    `(clojure.core/let [~de_ex ~value] ~@(for [[x y] syms] `(clojure.core/reset! ~x ~y)))))

(defn map-merge
  "merges maps recursively"
  [m1 m2]
  (if (and (map? m1) (map? m2))
    (merge-with map-merge m1 m2)
    m2))

(defn mapply
  "like map, but returns a vector with the original arguments too."
  [f coll & colls]
  (apply map
    (fn [& args]
      (conj (vec args) (apply f args)))
    (cons coll colls)))
