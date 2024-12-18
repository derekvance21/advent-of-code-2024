(ns advent-of-code.day1
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :as t]
   [clojure.java.io :as io]))

(defn parse-line
  [line]
  (mapv edn/read-string (str/split line #"\s+" 2)))

(defn transpose
  [m]
  (apply mapv vector m))

(defn columnize
  [lines]
  (transpose (map parse-line lines)))

(defn diff-sum
  [l r]
  (let [diffs (map (comp abs -) l r)]
    (reduce + 0 diffs)))

(defn similarity
  [l r]
  (let [freq (frequencies r)
        f (fn [x]
            (when-let [c (get freq x)]
              (* x c)))]
    (transduce (keep f) + 0 l)))

(defn solve
  [lines]
  (let [[l r] (columnize lines)]
    {:part1 (diff-sum (sort l) (sort r))
     :part2 (similarity l r)}))

(defn -main
  [file]
  (with-open [rdr (io/reader file)]
    (print
     (solve (line-seq rdr)))))

(t/deftest day1
  (let [lines ["3   4"
               "4   3"
               "2   5"
               "1   3"
               "3   9"
               "3   3"]
        {:keys [part1 part2]} (solve lines)]
    (t/testing "Part 1" (t/is (= 11 part1)))
    (t/testing "Part 2" (t/is (= 31 part2)))))

(t/run-test day1)
