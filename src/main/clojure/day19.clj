(ns day19
  (:require [clojure.string :refer [split starts-with?]]))

(def input (split (slurp "./src/main/clojure/inputs/day19.in") #"\n\n"))

(def count-matches
  (memoize
    (fn [patterns design]
      (if (empty? design)
        1
        (->> (filter (fn [pattern] (starts-with? design pattern)) patterns)
             (map (fn [pattern] (count-matches patterns (subs design (count pattern)))))
             (reduce +))))))

(defn solve [input]
  (let [[patterns designs] [(split (first input) #", ") (split (last input) #"\n")]]
    (->> (map (fn [design] (count-matches patterns design)) designs)
         (remove zero?)
         ((juxt count (partial reduce +))))))

(comment
  (time (solve input))
  )