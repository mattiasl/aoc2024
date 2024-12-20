(ns day20
  (:require [clojure.string :refer [split]]
            [clojure.math.combinatorics :refer [combinations]]))

(def input (split (slurp "./src/main/clojure/inputs/day20.in") #"\n"))

(defn parse-input [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(defn all-of-type [maze ch]
  (into #{} (keys (filter (fn [[_ v]] (= v ch)) maze))))

(defn create-maze [input]
  (let [all (parse-input input)]
    (->> (vals all)
         (distinct)
         (reduce (fn [a c] (assoc a c (all-of-type all c))) {}))))

(defn get-neighbours [pos]
  (map (fn [dir] (mapv + dir pos)) [[1 0] [0 -1] [-1 0] [0 1]]))

(defn make-dist-map [src walls]
  (loop [pos src
         dist 0
         visited #{src}
         dist-from-src {src dist}]
    (let [neighbours (->> (get-neighbours pos)
                          (remove visited)
                          (remove walls))
          neighbour (first neighbours)]
      (if (nil? neighbour)
        (assoc dist-from-src pos dist)
        (recur neighbour (inc dist) (conj visited pos) (assoc dist-from-src pos dist))))))

(defn count-cheats [dist-map pred? min-dist]
  (reduce (fn [cheats [[[ax ay] ad] [[bx by] bd]]]
            (let [md (+ (abs (- ax bx)) (abs (- ay by)))]
              (if (and (pred? md) (>= (apply - (conj ((juxt max min) ad bd) md)) min-dist))
                (inc cheats)
                cheats)))
          0
          (combinations dist-map 2)))

(defn solve [input pred? min-dist]
  (let [maze (create-maze input)
        from (first (maze \S))
        walls (maze \#)
        dist-map (make-dist-map from walls)]
    (count-cheats dist-map pred? min-dist)))

(defn part1? [md] (= md 2))
(defn part2? [md] (<= md 20))

(comment
  (time (solve input part1? 100))
  (time (solve input part2? 100))
  )