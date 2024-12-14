(ns day12
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day12.in") #"\n"))

(defn create-garden [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x ^char ch] (map-indexed vector row)]
                 {[x y] ch})))

(defn get-plats-by-type [garden plant]
  (keys (filter (fn [[_ v]] (= v plant)) garden)))

(defn get-neighbours
  ([pos] (get-neighbours (constantly true) pos))
  ([pred? pos]
   (->> (reduce (fn [a dir] (conj a (mapv + pos dir)))
                []
                [[1 0] [0 -1] [-1 0] [0 1]])
        (filter pred?))))

(defn flood-fill [garden pos]
  (let [plant (garden pos)]
    (loop [queue (list pos)
           visited #{}]
      (if (empty? queue)
        visited
        (let [pos (peek queue)
              plants (->> (get-neighbours pos)
                          (remove (fn [other] (contains? visited other)))
                          (filter (fn [other] (= plant (garden other)))))]
          (recur (into (pop queue) plants) (conj visited pos)))))))

(defn create-regions [garden plant]
  (-> (reduce (fn [regions pos]
                (if (contains? (regions :visited) pos)
                  regions
                  (let [plants (flood-fill garden pos)]
                    (-> (update regions :regions (fn [old] (conj old plants)))
                        (update :visited (fn [old] (clojure.set/union old plants)))))))
              {:regions #{} :visited #{}}
              (get-plats-by-type garden plant))
      (:regions)))

(defn perimeter [_ region]
  (let [region? (fn [neighbour] (contains? region neighbour))]
    (->> (map (fn [pos]
                (->> (get-neighbours region? pos)
                     (count)))
              region)
         (map (fn [x] (abs (- x 4))))
         (reduce +))))

(defn price [f regions]
  (->> (map (fn [region] (* (f region) (count region))) regions)
       (reduce +)))

(defn corner-pos [[x y] [x1 y1] [x2 y2]]
  [(if (= x x1) x2 x1) (if (= y y1) y2 y1)])

(defn corners [garden pos]
  (let [plant (garden pos)
        neighbours (get-neighbours pos)]
    (->> (take 4 (rest (partition 2 1 (cycle neighbours))))
         (map (fn [[a b]] [a b (corner-pos pos a b)]))
         (map (fn [x] (map garden x)))
         (filter (fn [[a b d]] (or (and (not= a plant) (not= b plant))
                                   (and (= a plant) (= b plant) (not= d plant)))))
         (count))))

(defn region-sides [garden region]
  (->> (map (fn [pos] (corners garden pos)) region)
       (reduce +)))

(defn solve [input f]
  (let [garden (create-garden input)]
    (->> (reduce (fn [a plant] (assoc a plant (create-regions garden plant)))
                 {}
                 (into #{} (vals garden)))
         (vals)
         (map (partial price (partial f garden)))
         (reduce +))))

(comment
  (time (solve input perimeter))
  (time (solve input region-sides))
  )