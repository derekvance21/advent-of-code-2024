(ns advent-of-code.day2
  (:require
   [clojure.java.io :as io]
   [advent-of-code.utils :as utils]))

(defn similarity
  [l r]
  (let [freq (frequencies r)
        f (fn [x]
            (when-let [c (get freq x)]
              (* x c)))]
    (transduce (keep f) + 0 l)))

(defn -main
  [input]
  (with-open [rdr (io/reader input)]
    (let [[l r] (utils/columnize (line-seq rdr))]
      (print (similarity l r)))))

