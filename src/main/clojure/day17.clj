(ns day17
  (:require [clojure.string :refer [split]]
            [clojure.math :refer [pow]]))

(def input (split (slurp "./src/main/clojure/inputs/day17.in") #"\n\n"))

(defn create-state [[registers code]]
  (let [reg (->> (re-seq #"\d+" registers)
                 (map read-string))
        program (->> (re-seq #"\d+" code)
                     (mapv read-string))]
    [reg program]))

(defn dv [op val]
  (long (quot op (pow 2 val))))

(defn adv [a b c v] [(dv a v) b c])
(defn bxl [a b c o] [a (bit-xor b o) c])
(defn bst [a _ c v] [a (mod v 8) c])
(defn bxc [a b c] [a (bit-xor b c) c])
(defn out [v] (mod v 8))
(defn bdv [a _ c v] [a (dv a v) c])
(defn cdv [a b _ v] [a b (dv a v)])

(defn run [[reg program]]
  (loop [[a b c] reg
         [ip op & prog] program
         output []]
    (if (nil? ip)
      output
      (let [v (case op 4 a 5 b 6 c op)]
        (case ip
          0 (recur (adv a b c v) prog output)
          1 (recur (bxl a b c op) prog output)
          2 (recur (bst a b c v) prog output)
          3 (recur [a b c] (if (zero? a) prog (drop op program)) output)
          4 (recur (bxc a b c) prog output)
          5 (recur [a b c] prog (conj output (out v)))
          6 (recur (bdv a b c v) prog output)
          7 (recur (cdv a b c v) prog output))))))

(defn part1 [input]
  (->> (create-state input)
       (run)
       (clojure.string/join ",")))

; with some hints from Tomas
(defn part2 [input]
  (let [[_ program] (create-state input)]
    (reduce (fn [a n]
              (let [expected (take-last n program)]
                (loop [a' (* a (long (pow 2 3)))]
                  (if (= (take-last n (run [[a' 0 0] program])) expected)
                    a'
                    (recur (inc a'))))))
            0
            (range (inc (count program))))))

(comment
  (time (part1 input))
  (time (part2 input))
  )