(ns tailrecursion.stoke.term
  (:require
    [clojure.zip                :as zip]
    [tailrecursion.stoke.edit   :as e]
    [tailrecursion.stoke.reader :as r]
    [tailrecursion.stoke.pp     :as pp]))

(defn pprint []
  (print "\033[2J")
  (let [post #(if (identical? %1 (zip/node (zip/node @e/point)))
                [:span [:pass "\033[38;5;154m"] %2 [:pass "\033[0m"]]
                %2)]
    (binding [pp/post-process post]
      (pp/pprint (zip/root (zip/node @e/point))))))

(defn -main [file]
  (e/read-file file)
  (loop []
    (pprint)
    (let [c (char (.read System/in))]
      (when-not (= \q c)
        (case c
          \p (pprint)
          \k (e/edit zip/up)
          \j (e/edit zip/down)
          \h (e/edit zip/left)
          \l (e/edit zip/right)
          \d (e/edit zip/remove)
          \c (e/edit zip/replace (r/read-string (pr-str (read))))
          \u (e/undo zip/up)
          \r (e/undo zip/down)
          \t (e/undo zip/left)
          \y (e/undo zip/right)
          nil)
        (recur)))))
