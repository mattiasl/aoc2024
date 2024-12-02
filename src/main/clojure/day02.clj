(ns day02
  (:require [clojure.string :refer [split]]))

(def reports (split (slurp "./src/main/clojure/inputs/day02.in") #"\n"))

(defn parse [report]
  (->> (split report #" ")
       (map read-string)))

(defn part1? [report]
  (let [deltas (into #{} (map - (rest report) (drop-last report)))]
    (or (clojure.set/subset? deltas #{-1 -2 -3}) (clojure.set/subset? deltas #{1 2 3}))))

(defn part2? [report]
  (->> (reduce (fn [a c] (conj a (keep-indexed (fn [i x] (if (not= i c) x)) report))) [] (range (count report)))
       (some part1?)))

(defn count-safe [safe? reports]
  (->> (map parse reports)
       (filter safe?)
       (count)))

(comment
  (time (map (fn [safe?] (count-safe safe? reports)) [part1? part2?]))
  )