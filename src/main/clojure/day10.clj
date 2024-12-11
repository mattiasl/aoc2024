(ns day10
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day10.in") #"\n"))

(defn create-map [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x ^char h] (map-indexed vector row)]
                 {[x y] (Character/digit h 10)})))

(defn get-trailheads [the-map]
  (keys (filter (fn [[_ v]] (= v 0)) the-map)))

(defn get-neighbours [pos]
  (reduce (fn [a dir]
            (conj a (mapv + pos dir)))
          #{}
          [[1 0] [0 -1] [-1 0] [0 1]]))

(defn find-trails [the-map trail trails]
  (let [pos (last trail)
        height (the-map pos)]
    (if (= height 9)
      (conj trails (conj trail pos))
      (let [neighbours (->> (get-neighbours pos)
                            (filter (fn [x] (= (the-map x) (inc height)))))]
        (reduce (fn [a neighbour]
                  (find-trails the-map (conj trail neighbour) a))
                trails
                neighbours)))))

(defn solver [input mapper-fn]
  (let [the-map (create-map input)]
    (->> (map (fn [trailhead]
                (->> (find-trails the-map [trailhead] #{})
                     (mapper-fn)
                     (count)))
              (get-trailheads the-map))
         (reduce +))))

(defn part1 [trails]
  (->> (map last trails)
       (distinct)))

(def part2 identity)

(comment
  (time (solver input part1))
  (time (solver input part2))
  )