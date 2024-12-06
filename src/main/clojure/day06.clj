(ns day06
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day06.in") #"\n"))

(defn create-map [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(defn size [input]
  [(count input) (count (first input))])

(defn all-of-type [ch the-map]
  (keys (filter (fn [[_ v]] (= v ch)) the-map)))

(def turn-right {[0 -1] [1 0] [1 0] [0 1] [0 1] [-1 0] [-1 0] [0 -1]})

(defn next-pos [pos dir] (mapv + pos dir))

(defn walk [[mx my] guard obstructions]
  (loop [pos guard
         dir [0 -1]
         visited {}]
    (let [pos' (next-pos pos dir)
          dirs-at-pos (get visited pos' #{})]
      (if (contains? dirs-at-pos dir)
        {:loop true :visited (keys visited)}
        (if (and (< -1 (first pos') mx) (< -1 (second pos') my))
          (if (contains? obstructions pos')
            (recur pos (turn-right dir) visited)
            (recur pos' dir (assoc visited pos' (conj dirs-at-pos dir))))
          {:loop false :visited (keys visited)})))))

(defn part1 [input]
  (let [the-map (create-map input)
        guard (first (all-of-type \^ the-map))
        obstructions (into #{} (all-of-type \# the-map))]
    (->> (walk (size input) guard obstructions)
         (:visited)
         (count))))

(defn part2 [input]
  (let [the-map (create-map input)
        guard (first (all-of-type \^ the-map))
        obstructions (into #{} (all-of-type \# the-map))
        map-size (size input)
        new-obs-candidates (into #{} (:visited (walk map-size guard obstructions)))]
    (->> (pmap (fn [obstruction]
                 (if (:loop (walk map-size guard (conj obstructions obstruction))) obstruction))
               (disj new-obs-candidates guard))
         (remove nil?)
         (count))))

(comment
  (time (part1 input))
  (time (part2 input))
  )