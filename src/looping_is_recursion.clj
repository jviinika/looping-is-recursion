(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn[acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a tail]
                 (if (empty? tail)
                   a
                   (recur (first tail) (rest tail))))]
        (helper (first a-seq) (rest  a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (and (empty? seq1) (not(empty? seq2))) false
        (and (not (empty? seq1)) (empty? seq2)) false
        (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         tail a-seq]
    (cond
      (empty? tail) nil
      (pred (first tail)) n
      :else (recur (inc n) (rest tail)))))

(defn avg [a-seq]
  (loop [counter 0
         cumulative 0
         items a-seq]
    (if (empty? items)
      (/ cumulative counter )
      (recur (inc counter) (+ cumulative (first items)) (rest items)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [toggles #{}
         items a-seq]
    (if (empty? items)
      toggles
      (recur (toggle toggles (first items)) (rest items)))))

(defn fast-fibo [n]
  (loop [f-1 0
         f 1
         counter 0]
    (if (= counter n)
      f-1
      (recur f (+ f-1 f) (inc counter)))))

(defn cut-at-repetition [a-seq]
  (loop [elements #{}
         items a-seq
         coll []]
    (if (or (empty? items) (contains? elements (first items)))
      coll
      (recur (toggle elements (first items)) (rest items) (conj coll (first items))))))

