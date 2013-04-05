(ns tailrecursion.stoke.edit
  (:require
    [clojure.zip                :as zip]
    [tailrecursion.stoke.reader :as r]
    [tailrecursion.stoke.pp     :as pp]))

(def undos (atom []))
(def point (atom "" :validator (complement nil?)))

(defn- pprint []
  (print "\033[2J")
  (let [post #(if (identical? %1 (zip/node @point))
                [:span [:pass "\033[38;5;154m"] %2 [:pass "\033[0m"]]
                %2)]
    (binding [pp/post-process post]
      (pp/pprint (zip/root @point)))))

(defn- read-file [f]
  (reset! point (zip/down (r/zipper (r/read-file f))))
  (pprint))

(defn- swap!* [f & args]
  (try
    (apply swap! point f args)
    (catch Throwable e)))

(defn- move [f]
  (fn moving
    ([] (moving 1))
    ([n]
     (dotimes [i n] (swap!* f))
     (pprint))))

(def o      read-file)
(def p      pprint)
(def k      (move zip/up))
(def j      (move zip/down))
(def h      (move zip/left))
(def l      (move zip/right))
(def d      (move zip/remove))

(defmacro c [form]
  `(do
     (swap!* zip/replace (r/read-string (pr-str '~form)))
     (pprint)))
