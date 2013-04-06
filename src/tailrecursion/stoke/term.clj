(ns tailrecursion.stoke.term
  (:require
    [clojure.zip                      :as zip]
    [tailrecursion.stoke.edit         :as e]
    [tailrecursion.stoke.read         :as r]
    [tailrecursion.stoke.print        :as pp]))

(def mode (atom :normal))

(def mult (atom nil))

(defn update-mult [c]
  (swap! mult #(let [i (Character/digit c 10)]
                 (if (not %) i (+ (* 10 %) i)))))

(defn mult-cmd [g f & args]
  (dotimes [i (or @mult 1)] (apply g f args))
  (reset! mult nil))

(defn input [] (r/read-string (pr-str (read))))

(defn insert-left       [z x] (-> (zip/insert-left z x) zip/left))
(defn insert-right      [z x] (-> (zip/insert-right z x) zip/right))
(defn insert-rightmost  [z x] (-> (zip/up z) (zip/append-child x) zip/down zip/rightmost))
(defn insert-leftmost   [z x] (-> (zip/up z) (zip/insert-child x) zip/down))
(defn remove-parent     [z]   (-> (zip/up z) zip/remove))
(defn remove-point      [z]   (-> (zip/remove z) zip/next))

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
          \x        (fn [_] (mult-cmd e/edit remove-point))
          \e        (fn [_] (e/read-file (str (read))))
          \u        (fn [_] (reset! mode :undo))
          \c        (fn [_] (e/edit zip/replace (input)))
          \i        (fn [_] (e/edit insert-left (input)))
          \I        (fn [_] (e/edit insert-leftmost (input)))
          \a        (fn [_] (e/edit insert-right (input)))
          \A        (fn [_] (e/edit insert-rightmost (input)))
          \o        (fn [_] (e/edit insert-right :break))
          \D        (fn [_] (e/edit remove-parent))
          }
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

