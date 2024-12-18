(ns day18
  (:require [clojure.string :refer [split]]
            [clojure.data.priority-map :refer [priority-map]]))

(def input (split (slurp "./src/main/clojure/inputs/day18.in") #"\n"))
(def +inf Integer/MAX_VALUE)

(defn create-map [input]
  (-> (reduce (fn [a c]
                (conj a (->> (re-seq #"\d+" c)
                             (mapv read-string))))
              []
              input)
      (zipmap (rest (range)))))

(defn get-neighbours [pos]
  (map (fn [dir] (mapv + dir pos)) [[1 0] [0 -1] [-1 0] [0 1]]))

(defn dijkstra [from size walls steps]
  (let [to [size size]]
    (loop [q (priority-map from 0)
           distances (into {} q)]
      (let [[u dist] (peek q)]
        (cond
          (= u to) dist
          (nil? u) +inf
          :else
          (let [result (->> (get-neighbours u)
                            (remove (fn [x] (<= (walls x +inf) steps)))
                            (filter (fn [[x y]] (and (<= 0 x size) (<= 0 y size))))
                            (reduce (fn [a neighbour]
                                      (let [alt (inc dist)
                                            dist (distances neighbour +inf)]
                                        (if (< alt dist)
                                          (assoc a neighbour alt)
                                          a)))
                                    {}))]
            (recur (into (pop q) result) (merge distances result))))))))

(defn part1 [input]
  (dijkstra [0 0] 70 (create-map input) 1024))

(defn part2 [input]
  (let [walls (create-map input)
        steps (->> (range (count walls) 1024 -1)
                   (pmap (fn [steps]
                           (when (not= (dijkstra [0 0] 70 walls steps) +inf) steps)))
                   (remove nil?)
                   (first))]
    (some (fn [[k v]] (if (= v (inc steps)) k)) walls)))

(comment
  (time (part1 input))
  (time (part2 input))
  )