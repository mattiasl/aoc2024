(ns day14
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day14.in") #"\n"))

(def size [101 103])

(defn make-robots [input]
  (->> (map (fn [x] (re-seq #"-?\d+" x)) input)
       (map (fn [x] (map read-string x)))
       (map (fn [[px py vx vy]] {:p [px py] :v [vx vy]}))))

(defn move [size robot times]
  (let [[vx vy] (robot :v)]
    (update robot :p (fn [[px py]]
                       [(mod (+ px (* vx times)) (first size))
                        (mod (+ py (* vy times)) (second size))]))))

(defn get-quadrant [robot [mx my]]
  (let [[x y] (robot :p)
        [half_mx half_my] [(/ mx 2) (/ my 2)]]
    (cond
      (and (< -1 x (dec half_mx)) (< -1 y (dec half_my))) 0
      (and (< half_mx x mx) (< -1 y (dec half_my))) 1
      (and (< -1 x (dec half_mx)) (< half_my y my)) 2
      (and (< half_mx x mx) (< half_my y my)) 3)))

(defn make-lookup [robots]
  (reduce (fn [a robot]
            (let [pos (robot :p)]
              (update a pos (fn [old] (if (nil? old) 1 (inc old))))))
          {}
          robots))

(defn create-image [robots]
  (let [lookup (make-lookup robots)]
    (reduce (fn [image y]
              (let [row (reduce (fn [row x] (conj row (get lookup [x y] " "))) [] (range (first size)))]
                (str image (clojure.string/join (conj row "\n")))))
            ""
            (range (second size)))))

(defn tree? [image]
  (clojure.string/includes? image "1111111111111111111111111111111"))

(defn part1 [input]
  (->> (make-robots input)
       (map (fn [robot] (move size robot 100)))
       (map (fn [robot] (get-quadrant robot size)))
       (remove nil?)
       (frequencies)
       (vals)
       (apply *)))

(defn part2 [input]
  (let [robots (make-robots input)]
    (->> (range)
         (pmap (fn [times]
                 (let [image (->> (map (fn [robot] (move size robot times)) robots)
                                  (create-image))]
                   (if (tree? image)
                     (let [_ (println image)]
                       times)))))
         (remove nil?)
         (first))))

(comment
  (time (part1 input))
  (time (part2 input))
  )