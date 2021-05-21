(ns bdrml2uzi.utils)

(declare equivalent?)

(defn- equivalent-sequential? [a b]
  (when (sequential? b)
    (try
      (let [limit (count a)]
        (loop [i 0]
          (if (< i limit)
            (if (equivalent? (nth a i)
                             (nth b i))
              (recur (inc i))
              false)
            true)))
      (catch js/Object _ false))))

(defn- equivalent-map? [a b]
  (when (associative? b)
    (let [keys* (keys a)]
      (loop [key (first keys*)
             rest (next keys*)]
        (if (equivalent? (get a key)
                         (get b key))
          (if rest
            (recur
              (first rest)
              (next rest))
            true)
          false)))))

(defn equivalent? [a b]
  (or (= a b)
      (cond
        (sequential? a) (equivalent-sequential? a b)
        (map? a) (equivalent-map? a b)
        :else (= a b))))
