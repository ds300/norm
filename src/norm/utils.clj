(ns norm.utils)

(defn pmap-chunked [n f coll]
  (apply concat 
    (pmap #(doall (map f %)) (partition-all n coll))))

(defn pmapcat [f coll]
  (apply concat
    (pmap f coll)))

(defn pmapcat-chunked [n f coll]
  (apply concat
    (pmap-chunked n f coll)))

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
    (let [ids (ref {})]
      (fn
        ([] @ids)
        ([elem]
          (or 
            (@ids elem)
            (dosync
              (or
                (@ids elem)
                (+ start_value -1
                  (count
                    (alter ids #(assoc % elem (+ start_value (count %))))))))))
        ([elem not_found]
          (@ids elem not_found))))))

(defn counter 
  ([start_value]  (let [n (atom start_value)]
                    (fn ([i] (swap! n + i))
                        ([] @n))))
  ([] (counter 0)))

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
