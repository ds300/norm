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

(defn unique-id-getter []
  (let [ids (ref {})]
    (fn
      ([] @ids)
      ([elem]
        (or 
          (@ids elem)
          (dosync
            (or
              (@ids elem)
              (dec (count (alter ids #(assoc % elem (count %))))))))))))

(defn indexify 
  ([coll]
    (indexify 0 coll))
  ([start coll]
    (map vector (iterate inc start) coll)))
