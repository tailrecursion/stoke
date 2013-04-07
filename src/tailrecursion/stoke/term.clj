(ns tailrecursion.stoke.term
  (:require
    [clojure.zip                :as zip]
    [clojure.string             :as string]
    [clojure.java.shell         :as shell]
    [tailrecursion.stoke.edit   :as e]
    [tailrecursion.stoke.read   :as r]
    [tailrecursion.stoke.print  :as pp]
    [tailrecursion.stoke.util   :as u]
    [tailrecursion.stoke.syntax :as s]))

(declare key-bindings pprint)

(def lines  (Integer/parseInt (System/getenv "LINES"))) 
(def cols   (Integer/parseInt (System/getenv "COLUMNS"))) 
(def mode   (atom :normal))
(def mult   (atom nil))

(defn update-mode! [x]
  (when (get @key-bindings x)
    (reset! mult nil) 
    (reset! mode x)))

(defn update-mult! [c]
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
(defn remove-point      [z]   (cond
                                (zip/right z)
                                (-> (zip/right z) u/set-mark zip/left zip/remove u/get-mark)
                                (zip/left z)
                                (-> (zip/left z) u/set-mark zip/right zip/remove u/get-mark)
                                :else (zip/remove z)))

(def key-bindings
  (atom {:normal
         {:dispatch #(if (Character/isDigit %) (update-mult! %) %) 
          \formfeed (fn [_] (pprint)) 
          \q        (constantly :quit) 
          \u        (fn [_] (update-mode! :undo))
          \d        (fn [_] (update-mode! :delete))
          \h        (fn [_] (mult-cmd e/edit zip/left))
          \^        (fn [_] (mult-cmd e/edit zip/leftmost))
          \l        (fn [_] (mult-cmd e/edit zip/right))
          \$        (fn [_] (mult-cmd e/edit zip/rightmost))
          \j        (fn [_] (mult-cmd e/edit zip/down))
          \k        (fn [_] (mult-cmd e/edit zip/up))
          \L        (fn [_] (mult-cmd e/edit zip/next))
          \H        (fn [_] (mult-cmd e/edit zip/prev))
          \x        (fn [_] (mult-cmd e/edit remove-point))
          \e        (fn [_] (e/read-file (str (read))))
          \c        (fn [_] (e/edit zip/replace (input)))
          \C        (fn [_] (e/edit zip/insert-child (input)))
          \i        (fn [_] (e/edit insert-left (input)))
          \I        (fn [_] (e/edit insert-leftmost (input)))
          \a        (fn [_] (e/edit insert-right (input)))
          \A        (fn [_] (e/edit insert-rightmost (input)))
          \o        (fn [_] (e/edit insert-right (r/read-string "\n\n")))
          \?        (fn [_]
                      (do
                        (binding [*print-meta* true]
                          (prn (zip/node (zip/node @@e/point))))
                        (.read System/in)))
          }
         :delete
         {:dispatch #(if (Character/isDigit %) (update-mult! %) %)
          \d        (fn [_] (do
                              (mult-cmd e/edit remove-parent)
                              (update-mode! :normal)))
          }
         :undo
         {:dispatch #(if (Character/isDigit %) (update-mult! %) %)  
          \h        (fn [_] (mult-cmd e/undo zip/left))
          \H        (fn [_] (mult-cmd e/undo zip/leftmost))
          \l        (fn [_] (mult-cmd e/undo zip/right))
          \L        (fn [_] (mult-cmd e/undo zip/rightmost))
          \j        (fn [_] (mult-cmd e/undo zip/down))
          \k        (fn [_] (mult-cmd e/undo zip/up))
          \n        (fn [_] (mult-cmd e/undo zip/next))
          \p        (fn [_] (mult-cmd e/undo zip/prev))}})) 

(defn status-line [text]
  (subs (apply str text (repeat cols "-")) 0 cols))

(defn status []
  (println (status-line (format "[%s] [%s]" (str @mode) @e/file))))

(defn pprint []
  (print "\033[2J\r")
  (let [colr  (fn [x]
                (let [c (if (= x :break) \u2588 "")]
                  [:span [:pass (str "\033[38;5;154m\f" c)] x [:pass "\033[0m"]])) 
        pnt   (zip/node (zip/node @@e/point))
        post  #(if (::point (meta %1)) (colr %2) %2)
        src   (with-out-str
                (-> (zip/node @@e/point)
                  (zip/edit vary-meta assoc ::point true)
                  (s/mark-point ::point)
                  (s/colorize ::point 154)
                  zip/root
                  pp/pprint))
        [x y] (->> (string/split src #"\n")
                (map-indexed #(format "\033[38;5;241m%4d\033[0m %s" (inc %1) %2))
                (split-with #(not (re-find #"\f" %))))
        nx    (count x)
        ny    (count y)
        xtra  (/ (- lines 2 (+ nx ny)) 2)
        pad   "~"
        padt  (repeat (int (Math/floor xtra)) pad)
        padb  (repeat (int (Math/ceil xtra)) pad)
        padx  (repeat (- ny nx) pad)
        pady  (repeat (- nx ny) pad)
        all   (concat padt padx x y pady padb)
        nw    (count all)
        over  (int (Math/ceil (/ (- nw lines) 2)))
        win   (->> (if (< 0 over) (drop over all) all) (take lines))]
    (->> win (string/join "\n") println)
    (status)))

(defn -main [f]
  (e/read-file f)
  (loop []
    (pprint) 
    (let [c (char (.read System/in))]
      (if (= c (char 27))
        (do (update-mode! :normal) (recur))
        (let [k ((get-in @key-bindings [@mode :dispatch]) c) 
              f (get-in @key-bindings [@mode k])]
          (if (or (not f) (not= :quit (f c))) (recur)))))))

