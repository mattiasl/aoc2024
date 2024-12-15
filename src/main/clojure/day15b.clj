(ns day15b
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day15.in") #"\n\n"))

(defn parse-warehouse [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(defn all-of-type [ch the-map]
  (into #{} (keys (filter (fn [[_ v]] (= v ch)) the-map))))

(defn widen [[x y]] [(* 2 x) y])

(def move->dir {\^ [0 -1] \> [1 0] \v [0 1] \< [-1 0]})

(defn make-warehouse [[warehouse moves]]
  (let [warehouse (parse-warehouse (split warehouse #"\n"))]
    {:robot (widen (first (all-of-type \@ warehouse)))
     :walls (->> (all-of-type \# warehouse)
                 (map (fn [p] (let [wp (widen p)] [wp (update wp 0 inc)])))
                 (apply concat)
                 (into #{}))
     :boxes (->> (all-of-type \O warehouse)
                 (map (fn [p] (let [wp (widen p)] [wp (update wp 0 inc)])))
                 (into #{}))
     :moves (->> (clojure.string/replace moves #"\n" "")
                 (map move->dir))}))

(defn get-box [warehouse position]
  (or (get-in warehouse [:boxes [position (update position 0 inc)]])
      (get-in warehouse [:boxes [(update position 0 dec) position]])))

(defn move-box [box dir]
  (mapv (fn [p] (mapv + p dir)) box))

(defn get-movable-boxes [warehouse dir pos]
  (let [x-move (zero? (second dir))]
    (loop [moveable-boxes {}
           pos pos]
      (let [new-pos (mapv + dir pos)
            box (get-box warehouse new-pos)]
        (cond
          (get-in warehouse [:walls new-pos]) :wall

          (and x-move box)
          (recur (assoc moveable-boxes box (move-box box dir)) (mapv + dir new-pos))

          box
          (let [left-boxes (get-movable-boxes warehouse dir (first box))
                right-boxes (get-movable-boxes warehouse dir (second box))]
            (if (or (= :wall left-boxes) (= :wall right-boxes))
              :wall
              (merge (assoc moveable-boxes box (move-box box dir)) left-boxes right-boxes)))
          :else
          moveable-boxes)))))

(defn move [warehouse]
  (reduce (fn [warehouse dir]
            (let [pos (warehouse :robot)
                  movable-boxes (get-movable-boxes warehouse dir pos)]
              (if (= movable-boxes :wall)
                warehouse
                (-> (assoc warehouse :robot (mapv + dir pos))
                    (update :boxes (fn [boxes] (apply disj boxes (keys movable-boxes))))
                    (update :boxes (fn [boxes] (apply conj boxes (vals movable-boxes))))))))
          warehouse
          (warehouse :moves)))

(defn part2 [input]
  (->> (make-warehouse input)
       (move)
       (:boxes)
       (map (fn [[[x y] _]] (+ x (* 100 y))))
       (reduce +)))

(comment
  (time (part2 input))
  )