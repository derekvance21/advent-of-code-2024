(ns advent-of-code.day4
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

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

;; stateful transducer - keep track of the previous (dec (count search)) items
;; on every input, check if previous is the search sequence you're looking for
;; if so, add it to result, otherwise, result
(defn search-by
  [f search]
  (fn [rf]
    (let [left (volatile! search)
          matched (volatile! [])]
      (fn
        ([] (rf))
        ([result]
         (let [ret (if (seq @left)
                     result
                     (unreduced (rf result @matched)))]
           (rf ret)))
        ([result input]
         (let [ret (if (seq @left)
                     result
                     (let [match @matched]
                       (vreset! matched [])
                       (vreset! left search)
                       (rf result match)))]
           (condp = (f input)
             (first @left) (do
                             (vswap! matched conj input)
                             (vswap! left rest))
             (first search) (do
                              (vreset! matched [input])
                              (vreset! left (rest search)))
             (do (vreset! left search)
                 (vreset! matched [])))
           ret))))))

(defn merge-diags
  ([] {})
  ([mm] mm)
  ([mm m]
   (merge-with #(merge-with into %1 %2) mm m)))

(into (sorted-map) [])

;; Shift+j joins current line to next line!

(defn transpose+
  [m f]
  (let [rf (completing
            (fn [t [i [j x]]]
              (update t i (fnil assoc (sorted-map)) j x)))
        xf (comp
            (map-indexed
             (fn [i row]
               (eduction
                (map-indexed (fn [j x]
                               [i [j x]]))
                row)))
            cat
            (map (fn [[i [j x]]]
                   [(f i j) [i x]])))]
    (transduce
     xf rf
     (sorted-map)
     m)))

;; maybe searches?
;; list of things to search.
;; then remove `left`
;; and if any things in searches are empty, then output x?
;; so when a line is done, empty that search
;; makes it kind of like ndfsm
#_(defn re
    [s left]
    (fn [x]
      (if (seq left)
        (condp = x
          (first left) (re s (rest left))
          (first s) (re s (rest s))
          (re s left))
        s)))

(defn search-diags
  [m s]
  (into []
        (comp
         (map (fn [[i r]]
                (into
                 []
                 (comp
                  (search-by val s)
                  (map first)
                  (map (fn [[j x]]
                         [i j x])))
                 r)))
         cat)
        m))

(defn part2
  [lines]
  (let [f (transpose+ lines +)
        b (transpose+ lines -)
        normalize-f (fn [[xf yf]]
                      [(inc yf) (dec (- xf yf))])
        normalize-b (fn [[xb yb]]
                      [(inc yb) (inc (- yb xb))])
        f-matches (into #{}
                        (map normalize-f)
                        (into (search-diags f "SAM") (search-diags f "MAS")))

        b-matches (into #{}
                        (map normalize-b)
                        (into (search-diags b "SAM") (search-diags b "MAS")))]
    (count (set/intersection f-matches b-matches))))

(part2 example)

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
    {:part1 (part1 diags)
     :part2 (part2 lines)}))

(solve example)

(defn -main
  [input & _]
  (with-open [rdr (io/reader input)]
    (let [lines (line-seq rdr)]
      (print (solve lines)))))

