(ns advent-of-code.day4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn transpose
  [m]
  (apply mapv vector m))

(defn mv->v
  [mv]
  (into [] (map val) (sort-by key mv)))

(defn merge-diags
  ([] {})
  ([mm] mm)
  ([mm m]
   (merge-with #(merge-with into %1 %2) mm m)))

(into (sorted-map) [])

;; Shift+j joins current line to next line!
((partial merge-with merge))
(defn transpose+
  [m f]
  (let [rf (partial merge-with merge #_into #_%&)
        row-xf (map-indexed
                (fn [i row]
                  (transduce
                   (map-indexed
                    (fn [j x]
                      {(f i j) {i x} #_[x]}))
                   rf (sorted-map) row)))]
    (transduce row-xf rf (sorted-map) m)))

;; stateful transducer - keep track of the previous (dec (count search)) items
;; on every input, check if previous is the search sequence you're looking for
;; if so, add it to result, otherwise, result
(defn index-of
  [m subseq]
  ()
  )

(transpose+
 [[1  2  3  4]

  [5  6  7  8]

  [9  10 11 12]]
 -
 #_(fn [x y]
     (+ (- x y) 3))
 #_-
 #_(fn [_ y] y))

(defn diag-equals
  [[xf yf] [xb yb]]
  (and (= yf yb)
       (= (- xf yf)
          (- yb xb))))

(diag-equals
 [5 2]
 [-1 2])
(diag-equals
 [3 2]
 [1 2])

(diag-equals
 3 [5 0] [-1 2])

(defn row-diag
  [i row]
  (transduce
   (map-indexed
    (fn [j x]
      {:f {(+ i j) [x]}
       :b {(- i j) [x]}
       :r {i [x]}
       :c {j [x]}}))
   merge-diags
   row))

(defn matrix-diags
  [m]
  (transduce (map-indexed row-diag) merge-diags m))

(comment
  (matrix-diags
   [[1 2]
    [3 4]]))

(def example
  ["MMMSXXMASM"
   "MSAMXMSMSA"
   "AMXSXMAAMM"
   "MSAMASMSMX"
   "XMASAMXAMM"
   "XXAMMXXAMA"
   "SMSMSASXSS"
   "SAXAMASAAA"
   "MAMMMXMMMM"
   "MXMXAXMASX"])

(defn xmas-matches
  [s]
  (count (re-seq #"XMAS" s)))

(defn part1
  [diags]
  (transduce
   (comp
    (map val)
    (mapcat vals)
    (map str/join)
    (mapcat (juxt identity str/reverse))
    (map xmas-matches))
   (completing +)
   diags))

(defn solve
  [lines]

  (let [diags (matrix-diags lines)]
    {:part1 (part1 diags)}))

(solve example)

(update-vals
 (matrix-diags
  example
  #_["M.S"
     ".A."
     "M.S"])
 (comp #(mapv str/join %) mv->v))

(defn -main
  [input & _]
  (with-open [rdr (io/reader input)]
    (let [lines (line-seq rdr)]
      (print (solve lines)))))

