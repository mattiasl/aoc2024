(ns day09)

(def input (slurp "./src/main/clojure/inputs/day09.in"))

(defn parse [input]
  (loop [index 0
         input input
         disk []]
    (let [[^char h & t] input]
      (if (nil? h)
        disk
        (let [val (Character/digit h 10)
              id (if (even? index) (/ index 2) -1)]
          (recur (inc index) t (conj disk (take val (repeat id)))))))))

(defn total-size [disk]
  (->> (partition 1 2 disk)
       (map first)
       (map count)
       (reduce +)))

(defn reversed-file-blocks [disk]
  (->> (remove (fn [x] (neg? x)) disk)
       (reverse)))

(defn defrag [disk]
  (let [size (total-size disk)
        flat-disk (flatten disk)]
    (loop [index 0
           defragged-disk (list)
           reversed-files (reversed-file-blocks flat-disk)
           flat-disk flat-disk]
      (if (= index size)
        (reverse defragged-disk)
        (let [block (first flat-disk)
              [block reversed-files] (if (neg? block)
                                       [(peek reversed-files) (pop reversed-files)]
                                       [block reversed-files])]
          (recur (inc index) (conj defragged-disk block) reversed-files (rest flat-disk)))))))

(defn enough-space? [block file]
  (<= (count file) (->> (filter neg? block)
                        (count))))

(defn write-file-to-block [blocks file]
  (let [old (take-while (complement neg?) blocks)]
    (concat old file (drop (+ (count old) (count file)) blocks))))

(defn maybe-move-file [disk file-id]
  (let [file (disk file-id)]
    (or (->> (filter (fn [[block-id blocks]] (and (< block-id file-id) (enough-space? blocks file))) disk) ; TODO make index for quicker lookup
             (map (fn [[block-id blocks]]
                    (-> (assoc disk block-id (write-file-to-block blocks file))
                        (assoc file-id (take (count file) (repeat -1))))))
             (first))
        disk)))

(defn checksum [disk]
  (->> (map-indexed (fn [x v] (* x (if (neg? v) 0 v))) disk)
       (reduce +)))

(defn part1 [input]
  (->> (parse input)
       (defrag)
       (checksum)))

(defn part2 [input]
  (let [disk (->> (parse input)
                  (zipmap (range))
                  (into (sorted-map)))
        files (->> (remove (fn [[_ v]] (or (empty? v) (neg? (first v)))) disk)
                   (keys)
                   (sort >))]
    (->> (reduce maybe-move-file disk files)
         (vals)
         (flatten)
         (checksum))))

(comment
  (time (part1 input))
  (time (part2 input))
  )