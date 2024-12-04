(ns day04
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day04.in") #"\n"))

(defn parse-words [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(defn all-of-type [ch words]
  (keys (filter (fn [[_ v]] (= v ch)) words)))

(defn get-star-shaped-points [pos]
  (let [dirs (for [x [-1 0 1] y [-1 0 1] :when (not= [x y] [0 0])] [x y])]
    (reduce (fn [a dir]
              (conj a (mapv (fn [x] (->> (mapv * x dir)
                                         (mapv + pos))) (for [d (range 1 4)] [d d]))))
            []
            dirs)))

(defn xmas? [words [m a s]]
  (and (= (words m) \M) (= (words a) \A) (= (words s) \S)))

(defn get-x-shaped-points [pos]
  (let [points [[[-1 -1] [1 1] [1 -1] [-1 1]]               ; top-left bottom-right top-right bottom-left
                [[1 1] [-1 -1] [1 -1] [-1 1]]               ; bottom-right top-left top-right bottom-left
                [[1 1] [-1 -1] [-1 1] [1 -1]]               ; bottom-right top-left bottom-left top-right
                [[-1 -1] [1 1] [-1 1] [1 -1]]]]             ; top-left bottom-right bottom-left top-right
    (reduce (fn [a points] (conj a (mapv (fn [p] (mapv + pos p)) points))) [] points)))

(defn Xmas? [words [ur dl ul dr]]
  (and (and (= (words ur) \M) (= (words dl) \S)) (and (= (words ul) \M) (= (words dr) \S))))

(defn word-search [input ch points-generator pred?]
  (let [words (parse-words input)]
    (->> (all-of-type ch words)
         (map points-generator)
         (map (fn [points] (->> (filter (partial pred? words) points)
                                (count))))
         (reduce +))))

(comment
  (time (word-search input \X get-star-shaped-points xmas?))
  (time (word-search input \A get-x-shaped-points Xmas?))
  )