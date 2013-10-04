;; The prime factors of 13195 are 5, 7, 13 and 29.

;; What is the largest prime factor of the number 600851475143 ?

(defn indivisible?
  [x y]
  (not= 0 (mod x y)))

(defn prime? [x]
  (cond
   (= x 2) true
   :else (let [sqrt (Math/sqrt x)
               numbers-smaller-than-sqrt (range 3 (inc sqrt) 2)]
           (and (indivisible? x 2)
                (every? (fn [n] (indivisible? x n)) numbers-smaller-than-sqrt)))))

(defn print-num [x] (print x))

(defn drop-predicate-maker [x]
  (fn [y]
    "Drops the number if it is not prime or if it is not a factor of x"
    (or
     (not (prime? y))
     (indivisible? x y))))

(defn largest-prime [x]
  (let [sqrt-num (Math/sqrt x)
        candidates (range (int (Math/floor sqrt-num)) 2 -1)]
    (first (drop-while (drop-predicate-maker x) candidates))))

(defn problem-3 []
  (largest-prime 600851475143))

(comment
  (problem-3)
)
