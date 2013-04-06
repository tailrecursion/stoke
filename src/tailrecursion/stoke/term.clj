(ns tailrecursion.stoke.term
  (:require
    [clojure.zip                      :as zip]
    [tailrecursion.stoke.edit         :as e]
    [tailrecursion.stoke.print        :as pp]))

(def mode (atom :normal))

(def mult (atom nil))

(defn update-mult [c]
  (swap! mult #(let [i (Character/digit c 10)]
                 (if (not %) i (+ (* 10 %) i)))))

(defn mult-cmd [g f & args]
  (dotimes [i (or @mult 1)] (apply g f args))
  (reset! mult nil))

(def key-bindings
  (atom {:normal
         {:dispatch #(if (Character/isDigit %) (update-mult %) %) 
          \q        (constantly :quit) 
          \h        (fn [_] (mult-cmd e/edit zip/left))
          \H        (fn [_] (mult-cmd e/edit zip/leftmost))
          \l        (fn [_] (mult-cmd e/edit zip/right))
          \L        (fn [_] (mult-cmd e/edit zip/rightmost))
          \j        (fn [_] (mult-cmd e/edit zip/down))
          \k        (fn [_] (mult-cmd e/edit zip/up))
          \n        (fn [_] (mult-cmd e/edit zip/next))
          \p        (fn [_] (mult-cmd e/edit zip/prev))
          \e        (fn [_] (e/read-file (str (read))))
          \u        (fn [_] (reset! mode :undo))}
         :undo
         {:dispatch #(if (Character/isDigit %) (update-mult %) %) 
          \h        (fn [_] (mult-cmd e/undo zip/left))
          \H        (fn [_] (mult-cmd e/undo zip/leftmost))
          \l        (fn [_] (mult-cmd e/undo zip/right))
          \L        (fn [_] (mult-cmd e/undo zip/rightmost))
          \j        (fn [_] (mult-cmd e/undo zip/down))
          \k        (fn [_] (mult-cmd e/undo zip/up))
          \n        (fn [_] (mult-cmd e/undo zip/next))
          \p        (fn [_] (mult-cmd e/undo zip/prev))}})) 

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
        (do (reset! mode :normal) (recur))
        (let [k ((get-in @key-bindings [@mode :dispatch]) c) 
              f (get-in @key-bindings [@mode k])]
          (if (or (not f) (not= :quit (f c))) (recur)))))))

