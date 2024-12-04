(ns advent-of-code.day2
  (:require
   [clojure.test :as t]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn parse-report
  [line]
  (mapv parse-long (str/split line #" ")))

(defn map-previous
  [f]
  (fn [rf]
    (let [pv (volatile! ::none)]
      (completing
       (fn [result input]
         (let [pval @pv]
           (vreset! pv input)
           (if (identical? pval ::none)
             result
             (rf result (f pval input)))))
       rf))))

(defn same?
  [f [x & rst]]
  (every? #(= % (f x)) (eduction (map f) rst)))

(defn safe?
  [report]
  (let [steps (into [] (map-previous -) report)]
    (and
     (same? (juxt neg? pos?) steps)
     (every? (comp #(<= 1 % 3) abs) steps))))

;; TODO - make this lazy
(defn remove-ones
  [coll]
  (loop
   [colls []
    pre  []
    coll coll]
    (if-let [[x & rst] coll]
      (recur
       (conj
        colls
        (into pre rst))
       (conj pre x)
       rst)
      colls)))

(defn solve
  [lines]
  (let [count-rf (completing (fn [c _] (inc c)))]
    {:part1 (transduce
             (comp
              (map parse-report)
              (filter safe?))
             count-rf
             0
             lines)
     :part2 (transduce
             (comp
              (map parse-report)
              (filter #(or (safe? %) (some safe? (remove-ones %)))))
             count-rf
             0
             lines)}))

(defn -main
  [input]
  (with-open [rdr (io/reader input)]
    (print (solve (line-seq rdr)))))

(def example
  ["7 6 4 2 1"
   "1 2 7 8 9"
   "9 7 6 2 1"
   "1 3 2 4 5"
   "8 6 4 4 1"
   "1 3 6 7 9"])

(comment
  (solve example))

(t/deftest day2
  (let [{:keys [part1 part2]} (solve example)]
    (t/testing "Part 1"
      (t/is (= 2 part1)))
    (t/testing "Part 2"
      (t/is (= 4 part2)))))

(t/run-test day2)
