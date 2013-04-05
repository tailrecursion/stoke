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
  (reset! point (zip/down (r/zipper (r/read-file f)))))

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

;;; In order for this to work without having to press enter, you must
;;; set stty before running the JVM:
;; stty -icanon min 1
(defn -main [file]
  (read-file file)
  (loop []
    (pprint)
    (let [c (char (.read System/in))]
      (when-not (= \q c)
        (case c
          \p (pprint)
          \k ((move zip/up))
          \j ((move zip/down))
          \h ((move zip/left))
          \l ((move zip/right))
          \d ((move zip/remove))
          \c (swap!* zip/replace (r/read-string (pr-str (read))))
          nil)
        (recur)))))
