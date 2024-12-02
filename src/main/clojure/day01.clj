(ns day01
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day01.in") #"\n"))

(defn split-and-transpose [input]
  (->> (map (fn [x] (re-seq #"\d+" x)) input)
       (map (fn [x] (map read-string x)))
       (apply mapv vector)))

(defn part1 [input]
  (->> (split-and-transpose input)
       (map sort)
       (apply map -)
       (map abs)
       (reduce +)))

(defn part2 [input]
  (let [[a b] (map frequencies (split-and-transpose input))]
    (->> (map (fn [x] (* x (b x 0))) (keys a))
         (reduce +))))

(comment
  (time (part1 input))
  (time (part2 input))
  )