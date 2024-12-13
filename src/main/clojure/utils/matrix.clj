(ns utils.matrix)

(defn sub-matrix [matrix [j i]]
  (reduce (fn [matrix row]
            (conj matrix (vec (keep-indexed #(if (not= %1 i) %2) row))))
          [] (keep-indexed #(if (not= %1 j) %2) matrix)))

(defn det [matrix]
  (if (= (count matrix) 1)
    (ffirst matrix)
    (->> (first matrix)
         (map-indexed (fn [column element]
                        (let [sign (if (= (mod column 2) 0) 1 -1)]
                          (* sign element (det (sub-matrix matrix [0 column]))))))
         (reduce +))))

(defn inverse [matrix]
  (let [d (det matrix)]
    (vec (map-indexed (fn [j row]
                        (vec (map-indexed (fn [i _]
                                            (let [sign (if (= (mod (+ i j) 2) 0) 1 -1)]
                                              (* (/ sign d) (det (sub-matrix matrix [i j])))))
                                          row)))
                      matrix))))

(defn transpose [m]
  (apply mapv vector m))

(defn multiply [a b]
  (let [b-transposed (transpose b)]
    (reduce (fn [m row-a]
              (conj m (mapv (fn [col-b]
                              (->> (mapv * row-a col-b)
                                   (reduce +))) b-transposed)))
            [] a)))