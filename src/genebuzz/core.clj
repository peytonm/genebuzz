(ns genebuzz.core)

(defn divisible
  "Determine whether one number is divisible by another."
  [x factor]
  (= 0 (mod x factor)))

(defn get-solution-element
  "Given a number, returns the number, Fizz, Buzz, or FizzBuzz according to FizzBuzz rules."
  [x]
  (cond
    (and (divisible x 3) (divisible x 5)) "FizzBuzz"
    (divisible x 3) "Fizz"
    (divisible x 5) "Buzz"
    :else x))

(defn calc-fitness
  "Calculate the fitness of an individual, given the solution."
  [xs solution]
  (apply + (map #(if (= % %2) 1 0) xs solution)))

(defn get-solution
  "Generate the solution to FizzBuzz, from start to end."
  [start end]
  (map get-solution-element (range start (inc end))))

(defn get-alleles
  "Generate the alleles, from start to end."
  [start end]
  (conj (range start (inc end)) "Fizz" "Buzz" "FizzBuzz"))

(defn create-individual
  "Randomly create an individual with a set number of genes drawn from alleles."
  [alleles num-genes]
  (repeatedly num-genes (partial rand-nth alleles)))

(defn flip
  "Return true with the specified probability."
  [prob]
  (> prob (rand)))

(defn mutate
  "Mutate an individual's genes with the specified probability."
  [individual prob alleles]
  (map #(if (flip prob) (rand-nth alleles) %) individual))

(defn cross-at
  "Swap an individual's genes at a certain cut-off. The cut-off is the last gene to be retained."
  [individual-a individual-b cut-point]
  [(concat (take cut-point individual-a) (drop cut-point individual-b))
   (concat (take cut-point individual-b) (drop cut-point individual-a))])

(defn cross
  "Cross genes from two individuals with the specified probability."
  [individual-a individual-b prob]
  (if (flip prob)
    (cross-at individual-a individual-b (inc (rand-int (dec (count individual-a)))))
    [individual-a individual-b]))

(defn create-population
  "Create n individuals with the specified number of genes and alleles."
  [n num-genes alleles]
  (repeatedly n (partial create-individual alleles num-genes)))

(defn calc-population-fitness
  "Calculate fitness across the population."
  [population solution]
  (map calc-fitness population (repeat solution)))

(defn get-elites
  "Returns the elites."
  [population fitness num-elites]
  ;; Note that if two individuals have the same fitness,
  ;; the choice between them will be basically arbitrary!
  (take num-elites
    (map :genes
      (sort-by :fitness >
        (map #(hash-map :genes %1 :fitness %2) population fitness)))))

(defn bin
  "Bin a number, given bin boundaries."
  [x bin-endpoints]
  (dec
    (eval
      (cons 'cond
        (interleave
          (map #(list '< x %) bin-endpoints)
          (range))))))

(defn sample
  "Take a weighted sample from a collection."
  [coll weights]
  (nth coll
    (bin
      (rand (apply + weights))
      (cons 0 (reductions + weights)))))

(defn breed
  "Select two parents and create a child."
  [population fitness cross-prob]
  ;; Note that an individual can breed with himself
  ;; since sampling is with replacement.
  (let [parents (repeatedly 2 #(sample population fitness))]
    (first
      (cross (first parents) (second parents) cross-prob))))

(defn evolve
  "Crosses and mutates the population to create n children."
  [population n fitness cross-prob mutate-prob alleles]
  (map mutate
    (repeatedly n
      #(breed population fitness cross-prob))
    (repeat n mutate-prob)
    (repeat n alleles)))

(defn advance-generation
  "Advances a generation by retaining elites and evolving."
  [population fitness num-elites cross-prob mutate-prob alleles]
  (concat
    (get-elites population fitness num-elites)
    (evolve population
      (- (count population) num-elites) fitness cross-prob mutate-prob alleles)))
