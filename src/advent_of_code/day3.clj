(ns advent-of-code.day3
  (:require
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

(defn switch
  [on? off?]
  (fn [rf]
    (let [switch (volatile! true)]
      (completing
       (fn
         [result input]
         (cond
           (on? input) (do (vreset! switch true) result)
           (off? input) (do (vreset! switch false) result)
           :else (if @switch
                   (rf result input)
                   result)))
       rf))))

(defn solve
  [s]
  (transduce
   (comp
    (map parse-instruction)
    (switch #{::on} #{::off}) ;; remove this for part 1
    (map #(apply * %)))
   +
   0
   (re-seq pattern s)))

(defn -main
  [input]
  (print (solve (slurp input))))

