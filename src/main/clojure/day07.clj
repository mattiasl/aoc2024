(ns day07
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day07.in") #"\n"))

(defn parse [input]
  (->> (map (fn [x] (re-seq #"\d+" x)) input)
       (map (fn [x] (map #(read-string %1) x)))))

(defn || [x y]
  (read-string (str x y)))

(defn valid?
  ([[expected & numbers] operators]
   (valid? expected (first numbers) (rest numbers) operators))
  ([expected actual numbers operators]
   (if (<= actual expected)
     (if (empty? numbers)
       (= actual expected)
       (true? (some true? (map (fn [op]
                                 (valid? expected (op actual (first numbers)) (rest numbers) operators))
                               operators))))
     false)))

(defn solve [input operators]
  (->> (parse input)
       (filter (fn [x] (valid? x operators)))
       (map first)
       (reduce +)))

(comment
  (time (solve input [* +]))
  (time (solve input [* + ||]))
  )