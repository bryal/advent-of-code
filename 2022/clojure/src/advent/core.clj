(ns advent.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn -main
  [& args]
  (day9))

(defn day9 []
  (let [input (parse (slurp "../inputs/day9.txt"))]
    (println "Part 1:" (simulate 2 input))
    (println "Part 2:" (simulate 10 input)))
  (defn simulate [n moves]
    "Simulate a rope with N knots, returning the number of unique points visited by the last knot."
    (->> moves
         (reduce (fn [[head tail visited] dir]
                   (let [head' (move-head head dir)
                         tail' (move-tail head head' tail)]
                     [head' tail' (conj visited (last tail'))]))
                 [[0 0] (repeat (dec n) [0 0]) #{[0 0]}])
         last
         count))
  (defn move-head [head dir]
    (mapv + head (cond (= dir \U) [0 1]
                       (= dir \D) [0 -1]
                       (= dir \L) [-1 0]
                       (= dir \R) [1 0])))
  (defn move-tail [head head' tail]
    (if (empty? tail)
      []
      (let [[tail0 & tail-rest] tail
            delta (mapv #(- %1 %2) head' tail0)
            tail0' (mapv #(+ (Integer/signum %1) %2) delta tail0)]
        (if (some #(> % 1) (mapv abs delta))
          (cons tail0' (move-tail tail0 tail0' tail-rest))
          tail))))
  (defn parse [input]
    (mapcat #(repeat (Integer/parseInt (subs % 2))
                     (first %))
            (str/split-lines input))))
