(ns genebuzz.core)

(defn divisible
  [x factor]
  (= 0 (mod x factor)))

(defn get-solution-element
  [x]
  (cond
  	(and (divisible x 3) (divisible x 5)) "FizzBuzz"
  	(divisible x 3) "Fizz"
  	(divisible x 5) "Buzz"
  	:else x))

(defn fitness
  [xs solution]
  (apply + (map #(if (= % %2) 1 0) xs solution)))

(defn get-solution
  [start end]
  (map get-solution-element (range start (inc end))))

(defn get-alleles
  [start end]
  (conj (range start (inc end)) "Fizz" "Buzz" "FizzBuzz"))

(defn create-individual
  [alleles num-genes]
  (repeatedly num-genes (partial rand-nth alleles)))

(defn flip
  [prob]
  (> prob (rand)))

(defn mutate
  [individual prob alleles]
  (map #(if (flip prob) (rand-nth alleles) %) individual))

(defn cross-at
  [individual-a individual-b cut-point]
  [(concat (take cut-point individual-a) (drop cut-point individual-b))
   (concat (take cut-point individual-b) (drop cut-point individual-a))])

(defn cross
  [individual-a individual-b prob]
  (if (flip prob)
  	(cross-at individual-a individual-b (inc (rand-int (dec (count individual-a)))))
  	[individual-a individual-b]))
