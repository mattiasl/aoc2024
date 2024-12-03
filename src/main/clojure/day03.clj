(ns day03)

(def input (slurp "./src/main/clojure/inputs/day03.in"))

(defn part1 [input]
  (->> (re-seq #"(?s)mul\(\d{1,3},\d{1,3}\)" input)
       (map (fn [x] (->> (re-seq #"\d+" x)
                         (map read-string)
                         (apply *))))
       (reduce +)))

(defn part2 [input]
  (part1 (clojure.string/replace input #"(?s)don't\(\).*?do\(\)" "")))

(comment
  (time (part1 input))
  (time (part2 input))
  )