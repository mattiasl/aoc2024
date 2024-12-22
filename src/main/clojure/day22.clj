(ns day22
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day22.in") #"\n"))

(defn mix [secret value]
  (bit-xor value secret))

(defn prune [secret]
  (mod secret 16777216))

(defn mix-and-prune [secret value]
  (->> (mix secret value)
       (prune)))

(defn next-secret-number [secret0]
  (let [secret1 (mix-and-prune secret0 (* secret0 64))
        secret2 (mix-and-prune secret1 (quot secret1 32))]
    (mix-and-prune secret2 (* secret2 2048))))

(defn part1 [input]
  (->> (map read-string input)
       (map (fn [seed] (->> (iterate next-secret-number seed)
                            (drop 2000)
                            (first))))
       (reduce +)))

(defn price [secret]
  (mod secret 10))

(defn change-seq->price [coll]
  (->> (partition 2 1 coll)
       (reduce (fn [[_ a] c]
                 (let [[p1 p2] (map price c)]
                   [p2 (conj a (- p2 p1))]))
               [-1 []])))

(defn get-all-first-sequence-matches [secret]
  (->> (iterate next-secret-number secret)
       (take 2000)
       (partition 5 1)
       (map change-seq->price)
       (reduce (fn [a [p s]] (if (contains? a s) a (assoc a s p)))
               {})))

(defn part2 [input]
  (->> (map read-string input)
       (reduce (fn [a secret]
                 (->> (get-all-first-sequence-matches secret)
                      (reduce-kv (fn [m k v]
                                   (update m k (fn [old] (if (nil? old) v (+ v old)))))
                                 a)))
               {})
       (vals)
       (apply max)))

(comment
  (time (part1 input))
  (time (part2 input))
  )