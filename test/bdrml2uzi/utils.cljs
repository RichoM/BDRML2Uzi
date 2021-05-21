(ns bdrml2uzi.utils)

(declare equivalent?)

(defn- equivalent-sequential? [a b]
  (when (sequential? b)
    (every? (fn [[i v]]
              (equivalent? v (nth b i)))
            (map-indexed (fn [i e] [i e]) a))))

(defn- equivalent-map? [a b]
  (when (associative? b)
    (every? (fn [[k v]]
              (equivalent? v (get b k)))
            a)))

(defn- equivalent-set? [a b]
  (every? (fn [v]
            (or (contains? b v)
                (some (fn [v'] (equivalent? v v'))
                      b)))
          a))

(defn equivalent? [a b]
  (or (identical? a b)
      (= a b)
      (cond
        (sequential? a) (equivalent-sequential? a b)
        (map? a) (equivalent-map? a b)
        (set? a) (equivalent-set? a b)
        :else (= a b))))
