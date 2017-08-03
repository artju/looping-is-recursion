(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [sequ n]
                 (if (or (empty? sequ) (= n 1))
                 (first sequ)
                   (recur (rest sequ) (dec n))))]
    (helper a-seq (count a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq-1 seq-2 len]
                 (cond
                  (= len 0) true
                   (not (= (count seq-1) (count seq-2))) false

                   (or (empty? seq-1) (empty? seq-2)) false

                   (= (first seq-1) (first seq-2)) (recur (rest seq-1) (rest seq-2) (dec len))
                   :else false))]
    (helper seq1 seq2 (count seq1))))


(defn find-first-index [pred a-seq]
  (loop [preds pred
         seq1 a-seq
         ind 0]
      (cond
      (empty? seq1) nil
        (preds (first seq1)) ind

        :else (recur preds (rest seq1) (inc ind))
        )))

(defn avg [a-seq]
  (loop [nums 0
         sum 0
         seq1 a-seq]
    (if (empty? seq1)
      (if (> (rem sum nums) 0)
        (float (/ sum nums))
        (int (/ sum nums)))
      (recur (inc nums) (+ (first seq1) sum) (rest seq1)))))

(defn parity [a-seq]
  (loop [seq1 a-seq
         set1 #{}
         len (count a-seq)]
    (cond
      (= len 0) set1
      (contains? set1 (first seq1)) (recur (rest seq1) (disj set1 (first seq1))  (dec len))
      :else (recur (rest seq1) (conj set1 (first seq1)) (dec len)))))

(defn fast-fibo [n]
  (loop [eka 1
         toka 1
         ind n]
    (cond
      (= n 0) n
      (< n 3) 1
      (and (= ind 2) (> eka toka)) eka
      (and (= ind 2) (> toka eka)) toka
      (= (rem ind 2) 0) (recur (+ eka toka) toka (dec ind))
      (> (rem ind 2) 0) (recur eka (+ eka toka) (dec ind))
      )))

(defn cut-at-repetition [a-seq]
  (loop [seq1 a-seq
         items []
         found #{}]
  (cond
    (empty? seq1) items
    (contains? found (first seq1)) items
    :else (recur (rest seq1) (conj items (first seq1)) (conj found (first seq1))
    ))))

