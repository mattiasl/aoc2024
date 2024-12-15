(ns day15
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day15.in") #"\n\n"))
(def input (split (slurp "./src/main/clojure/inputs/day15.test.in") #"\n\n"))

(defn parse-warehouse [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(defn all-of-type [ch the-map]
  (into #{} (keys (filter (fn [[_ v]] (= v ch)) the-map))))

(defn parse [[warehouse moves]]
  (let [warehouse (parse-warehouse (split warehouse #"\n"))]
    {:robot (first (all-of-type \@ warehouse))
     :walls (all-of-type \# warehouse)
     :boxes (all-of-type \O warehouse)
     :moves (clojure.string/replace moves #"\n" "")}))

(def moves {\^ [0 -1] \> [1 0] \v [0 1] \< [-1 0]})

(defn get-movable-boxes [warehouse dir moveable-boxes pos]
  (let [new-pos (mapv + dir pos)]
    (cond
      (get-in warehouse [:walls new-pos]) :wall
      (get-in warehouse [:boxes new-pos])
      (recur warehouse dir (assoc moveable-boxes new-pos (mapv + dir new-pos)) new-pos)
      :else
      moveable-boxes
      )
    )
  )

(defn move [warehouse]
  (reduce (fn [warehouse move]
            (let [dir (moves move)
                  pos (warehouse :robot)
                  movable-boxes (get-movable-boxes warehouse dir {} pos)
                  ]
              (if (= movable-boxes :wall)
                warehouse
                (-> warehouse
                    (assoc :robot (mapv + dir pos))
                    (update :boxes (fn [boxes] (apply disj boxes (keys movable-boxes))))
                    (update :boxes (fn [boxes] (apply conj boxes (vals movable-boxes))))
                    )
                )
              )
            )
          warehouse
          (warehouse :moves)
          )
  )

(defn part1 [input]
  (->> (parse input)
       (move)
       (:boxes)
       (map (fn [[x y]] (+ x (* 100 y))))
       (reduce +)
       )
  )

(comment
  (part1 input)
  )