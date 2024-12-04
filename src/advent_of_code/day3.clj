(ns advent-of-code.day3
  (:require
   [clojure.test :as t]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(def mul-re
  #"mul\((\d+),(\d+)\)")

(def pattern
  (re-pattern (str/join "|" [mul-re #"do\(\)" #"don't\(\)"])))

(defn parse-instruction
  [[s a b]]
  (case s
    "do()" ::on
    "don't()" ::off
    [(edn/read-string a)
     (edn/read-string b)]))

(defn valve
  [on? off?]
  (fn [rf]
    (let [open? (volatile! true)]
      (completing
       (fn
         [result input]
         (cond
           (on? input) (do (vreset! open? true) result)
           (off? input) (do (vreset! open? false) result)
           :else (if @open?
                   (rf result input)
                   result)))

       rf))))

(def part1-xf
  (comp
   (map parse-instruction)
   (remove #{::on ::off})
   (map #(apply * %))))

(def part2-xf
  (comp
   (map parse-instruction)
   (valve #{::on} #{::off})
   (map #(apply * %))))

(defn solve
  [s]
  (let [tokens (re-seq pattern s)]
    {:part1 (transduce part1-xf + 0 tokens)
     :part2 (transduce part2-xf + 0 tokens)}))

(defn -main
  [input]
  (print (solve (slurp input))))

(t/deftest day3
  (let [example "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
        {:keys [part1 part2]} (solve example)]
    (t/testing "Part 1" (t/is (= 161 part1)))
    (t/testing "Part 2" (t/is (= 48 part2)))))

(t/run-test day3)

(comment
  (into
   []
   (comp
    (valve #{::on} #{::off}))
   [::on 12 43 8 ::off 23 ::on 1 ::off 90 90 ::off 12 ::on 91 ::on 2 ::on]))
