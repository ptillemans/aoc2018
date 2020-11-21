(ns day1
  (:require [clojure.string :as str]))

(defn get-data
  "get the input data for day1"
  []
  (->>
   (slurp "input/day1.txt")
   (str/split-lines)
   (map #(Integer/parseInt %))))

(defn answer1
  "return the last frequency"
  []
  (let [freqs (reductions + 0 (get-data))]
    (last freqs)))

(defn find-first-duplicated
  "return the first element which is duplicated"
  ([fs]
   (find-first-duplicated #{} fs))
  ([seen [f & fs]]
   (cond
     (contains? seen f) f
     (empty? fs) nil
     :else (recur (conj seen f) fs))))

(defn answer2
  "return the first repeating frequency"
  []
  (->> (get-data)
       (cycle)
       (reductions + 0)
       (find-first-duplicated)))
