(ns day13
  (:require [clojure.string :refer [split]]
            [utils.matrix :refer [inverse multiply]]))

(def input (split (slurp "./src/main/clojure/inputs/day13.in") #"\n\n"))

(defn parse [maybe-adjust-price claw-machine]
  (->> (re-seq #"(?s)\d+" claw-machine)
       (map read-string)
       (maybe-adjust-price)))

(defn cost [[a b]]
  (+ (* a 3) b))

(defn prize-adjustment [adjustment claw-machine]
  (let [[ax ay bx by px py] claw-machine]
    [ax ay bx by (+ px adjustment) (+ py adjustment)]))

;Linear system {a*x1+b*x2=px, a*y1+b*y2=py} is solved with matrix operations: A*X=P => X=A⁻¹*P
(defn find-combo [[x1 y1 x2 y2 px py]]
  (-> (inverse [[x1 x2]
                [y1 y2]])
      (multiply [[px] [py]])
      (flatten)))

(defn solve [input]
  (->> (map find-combo input)
       (filter (fn [[a b]] (and (integer? a) (integer? b))))
       (map cost)
       (reduce +)))

(comment
  (time (solve (map (partial parse (partial prize-adjustment 0)) input)))
  (time (solve (map (partial parse (partial prize-adjustment 10000000000000)) input)))
  )