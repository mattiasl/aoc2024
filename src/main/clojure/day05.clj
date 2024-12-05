(ns day05
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day05.in") #"\n\n"))

(defn string->ints [s]
  (->> (re-seq #"\d+" s)
       (map read-string)))

(defn parse-rules [rules]
  (reduce (fn [a c]
            (let [[x y] (string->ints c)]
              (assoc a x (conj (get a x #{}) y))))
          {}
          (split rules #"\n")))

(defn parse-updates [updates]
  (->> (split updates #"\n")
       (map string->ints)))

(defn valid? [rules pages]
  (loop [pages (reverse pages)]
    (let [[page & pages] pages]
      (if (nil? pages)
        true
        (if (empty? (clojure.set/intersection (rules page) (set pages)))
          (recur pages)
          false)))))

(defn solver [input pred?]
  (let [rules (parse-rules (first input))]
    (->> (parse-updates (second input))
         (filter (partial pred? rules))
         (map (fn [x] (sort (fn [a b] (if (contains? (rules a) b) -1 1)) x)))
         (map (fn [v] (nth v (quot (count v) 2))))
         (reduce +))))

(comment
  (time (solver input valid?))
  (time (solver input (complement valid?)))
  )