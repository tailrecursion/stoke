(ns tailrecursion.stoke.edit
  (:require
    [clojure.zip :as z]))

(def point
  (atom (z/seq-zip '()) :validator (complement nil?)))

(defn read-file [f]
  (reset! point (z/down (z/seq-zip (read-string (str "(" (slurp f) ")"))))))
