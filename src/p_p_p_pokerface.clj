(ns p-p-p-pokerface)

(defn suit [[_ snd]]
  (str snd))

(defn rank [[fst _]]
  (let [special-rank {\T 10, \J 11, \Q 12, \K 13 \A 14}]
    (if (contains? special-rank fst)
      (get special-rank fst)
      (Integer/valueOf (str fst)))))

(defn n-of-a-kind [hand n]
  (= n (apply max (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [min-max (vals (frequencies (map rank hand)))]
    (and (= 2 (apply min min-max)) (= 3 (apply max min-max)))))

(defn two-pairs? [hand]
  (or
    (= [2 2 1] (vals (frequencies (map rank hand))))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
