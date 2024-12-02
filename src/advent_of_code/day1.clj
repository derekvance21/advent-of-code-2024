(ns advent-of-code.day1
  (:require
   [clojure.java.io :as io]
   [advent-of-code.utils :as utils]))

(defn diff-sum
  [l r]
  (let [diffs (map (comp abs -) l r)]
    (reduce + 0 diffs)))

(defn -main
  [file]
  (with-open [rdr (io/reader file)]
    (let [[l r] (utils/columnize (line-seq rdr))]
      (print (diff-sum (sort l) (sort r))))))

