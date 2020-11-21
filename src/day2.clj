(ns day2
  (:require [clojure.string :as str]
            [clojure.test :as test]))

(defn get-data
  "get the input data for day1"
  []
  (->>
   (slurp "input/day2.txt")
   (str/split-lines)))

(defn has-n-elements?
  "return true if seq has n elements"
  [n xs]
  (= n (count xs)))

(defn map-has-value-with-n-elements?
  [n m]
  (some identity
        (map #(has-n-elements? n (second %)) m)))

(test/with-test
  (defn has-n-lettergroup?
    "return true if a letter occurs n times"
    [n s]
    (->> (group-by identity s)
         (map-has-value-with-n-elements? n)))

  (test/is (not (has-n-lettergroup? 2 "abcdef")))
  (test/is (has-n-lettergroup? 2 "bababc"))
  (test/is (has-n-lettergroup? 3 "bababc"))
  (test/is (has-n-lettergroup? 2 "abbcde")))

(defn count-by
  "count element satisfying predicate"
  [f xs]
  (->> xs
       (map f)
       (filter identity)
       (count)))

(defn answer1
  []
  (let [codes (get-data)
        codes-2 (count-by #(has-n-lettergroup? 2 %) codes)
        codes-3 (count-by #(has-n-lettergroup? 3 %) codes)]
    (* codes-2 codes-3)))

(defn remove-char-at-n
  [n s]
  (str (subs s 0 n) (subs s (inc n))))

(defn find-duplicates-after-map
  [f xs]
  (->> xs
       (group-by f)
       (filter #(> (count (second %)) 1))
       (keys)))

(def test-boxes ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(test/with-test
  (defn find-close-boxes
    [codes]
    (let [positions (range (count (first codes)))]
      (->> positions
           (pmap #(find-duplicates-after-map (partial remove-char-at-n %) codes))
           (apply concat))))
  (test/is (= '("fgij") (find-close-boxes test-boxes))))

(defn answer2
  []
  (find-close-boxes (get-data)))

(comment
  (test/run-tests))
