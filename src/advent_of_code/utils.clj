(ns advent-of-code.utils
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.edn :as edn]))

(defn parse-line
  [line]
  (mapv edn/read-string (str/split line #"\s+" 2)))

(defn transpose
  [m]
  (apply mapv vector m))

(defn columnize
  [lines]
  (transpose (map parse-line lines)))

(defn -main
  [file]
  (with-open [rdr (io/reader file)]
    (let [[l r] (columnize (line-seq rdr))
          diffs (map (comp abs -) (sort l) (sort r))
          sum (reduce + 0 diffs)]
      (print sum))))

