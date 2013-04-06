(ns tailrecursion.stoke.term
  (:require
    [clojure.zip                      :as zip]
    [tailrecursion.stoke.edit         :as e]
    [tailrecursion.stoke.print        :as pp]
    [tailrecursion.stoke.mode.normal  :as n]))

(declare cmd normal-cmd)

(def mode (atom ::normal))

(defmulti cmd (fn [_] @mode))
(defmethod cmd ::normal [c] (n/normal-cmd c))

(defn pprint []
  (print "\033[2J\r")
  (let [post #(if (identical? %1 (zip/node (zip/node @@e/point)))
                [:span [:pass "\033[38;5;154m"] %2 [:pass "\033[0m"]]
                %2)]
    (binding [pp/post-process post]
      (pp/pprint (zip/root (zip/node @@e/point))))))

(defn -main []
  (loop []
    (pprint)
    (let [c (char (.read System/in))]
      (if (= c (char 27))
        (do (reset! mode ::normal) (recur))
        (when-not (= ::quit (cmd c)) (recur))))))

