(ns advent-of-code.utils
  (:require
   [clojure.string :as str]
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

