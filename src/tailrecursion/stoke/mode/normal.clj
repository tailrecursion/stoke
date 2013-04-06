(ns tailrecursion.stoke.mode.normal
  (:require
    [clojure.zip                      :as zip]
    [tailrecursion.stoke.edit         :as e]
    [tailrecursion.stoke.read         :as r]))

(def mult (atom nil))

(defn edit [f & args]
  (dotimes [i (or @mult 1)]
    (apply e/edit f args))
  (reset! mult nil))

(defmulti normal-cmd (fn [c] (if (Character/isDigit c) ::mult c)))

(defmethod normal-cmd ::mult [c]
  (swap! mult #(let [i (Character/digit c 10)] 
                 (if (not %) i (+ (* 10 %) i)))))

(defmethod normal-cmd \q [_] :tailrecursion.stoke.term/quit)
(defmethod normal-cmd \h [_] (edit zip/left))
(defmethod normal-cmd \H [_] (edit zip/leftmost))
(defmethod normal-cmd \l [_] (edit zip/right))
(defmethod normal-cmd \L [_] (edit zip/rightmost))
(defmethod normal-cmd \j [_] (edit zip/down))
(defmethod normal-cmd \k [_] (edit zip/up))
(defmethod normal-cmd \n [_] (edit zip/next))
(defmethod normal-cmd \p [_] (edit zip/prev))
(defmethod normal-cmd \e [_] (e/read-file (str (read))))
