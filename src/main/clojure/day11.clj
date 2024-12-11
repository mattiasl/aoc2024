(ns day11
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day11.in") #" "))

(def blink
  (memoize
    (fn [stone blinks]
      (if (= blinks 0)
        1
        (cond
          (= stone "0") (blink "1" (dec blinks))
          (odd? (count stone)) (blink (str (* 2024 (read-string stone))) (dec blinks))
          :else
          (let [div (/ (count stone) 2)]
            (+ (blink (subs stone 0 div) (dec blinks))
               (blink (str (Long/parseLong (subs stone div))) (dec blinks)))))))))


(defn part1 [input times]
  (->> (reduce (fn [stones _] (flatten (map apply-rule stones)))
               input
               (range times))
       (count)))

(defn solve [input blinks]
  (->> (map (fn [stone] (blink stone blinks)) input)
       (reduce +)))

(comment
  (time (solve input 25))
  (time (solve input 75))
  )