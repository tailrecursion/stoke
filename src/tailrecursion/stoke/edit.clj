(ns tailrecursion.stoke.edit
  (:require
    [clojure.zip                :as zip]
    [tailrecursion.stoke.reader :as r]
    [tailrecursion.stoke.pp     :as pp]))

(def mode  (atom :normal))
(def point (atom "" :validator (complement nil?)))

(defn pprint []
  (let [post #(if (identical? %1 (zip/node @point))
                [:span [:pass "\033[38;5;154m"] %2 [:pass "\033[0m"]]
                %2)]
    (binding [pp/post-process post]
      (pp/pprint (zip/root @point)))))

(defn read-file [f]
  (reset! point (zip/down (r/zipper (r/read-file f))))
  (pprint))

(defn move [f]
  (fn moving
    ([] (moving 1))
    ([n]
     (dotimes [i n]
       (try
         (swap! point f)
         (catch Throwable e))) 
     (pprint))))

(def up     (move zip/up))
(def down   (move zip/down))
(def left   (move zip/left))
(def right  (move zip/right))
(def rm     (move zip/remove))

(defn replace [s]
  (try
    (swap! point zip/replace (r/read-string s))
    (catch Throwable e))
  (pprint))
