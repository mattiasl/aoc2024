(ns day08
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day08.in") #"\n"))

(defn frequency-map [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x ch] (map-indexed vector row)]
                 (if (not= \. ch) {[x y] ch} {}))))

(defn size [input]
  [(count input) (count (first input))])

(defn frequency->pos [frequency-map]
  (reduce (fn [a [k v]] (let [existing (get a v #{})]
                          (assoc a v (conj existing k))))
          {}
          frequency-map))

(defn antinode-seq [pos dir]
  (lazy-seq (cons pos (antinode-seq (mapv + pos dir) dir))))

(defn invert [dir]
  (mapv * dir [-1 -1]))

(defn inside-map? [[mx my] [x y]]
  (and (< -1 x mx) (< -1 y my)))

(defn find-antinodes [f pred? antennas]
  (->> (combo/combinations antennas 2)
       (reduce (fn [a [a1 a2]]
                 (let [v (map - a1 a2)]
                   (clojure.set/union a
                                      (into #{} (f (take-while pred? (antinode-seq a1 v))))
                                      (into #{} (f (take-while pred? (antinode-seq a2 (invert v))))))))
               #{})))

(defn solve [input f]
  (->> (frequency-map input)
       (frequency->pos)
       (vals)
       (map (partial find-antinodes f (partial inside-map? (size input))))
       (apply clojure.set/union)
       (count)))

(comment
  (time (solve input (fn [x] (take 1 (drop 1 x)))))
  (time (solve input identity))
  )