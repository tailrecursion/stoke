(ns tailrecursion.stoke.term
  (:require
    [clojure.zip                :as zip]
    [clojure.string             :as string]
    [clojure.java.shell         :as shell]
    [tailrecursion.stoke.edit   :as e]
    [tailrecursion.stoke.read   :as r]
    [tailrecursion.stoke.print  :as pp]
    [tailrecursion.stoke.util   :as u]
    [tailrecursion.stoke.cmd    :as c]
    [tailrecursion.stoke.syntax :as s]))

(declare key-bindings pprint)

(def lines  (Integer/parseInt (System/getenv "LINES"))) 
(def cols   (Integer/parseInt (System/getenv "COLUMNS"))) 
(def mode   (atom :normal))
(def mult   (atom nil))

(defn set-mode! [x]
  (when (get @key-bindings x)
    (reset! mult nil) 
    (reset! mode x)))

(defn update-mult! [c]
  (swap! mult #(let [i (Character/digit c 10)]
                 (if (not %) i (+ (* 10 %) i)))))

(def mult-dispatch #(if (Character/isDigit %) (update-mult! %) %)) 

(defn mult-cmd [g f & args]
  (dotimes [i (or @mult 1)] (apply g f args))
  (reset! mult nil))

(defn input []
  (r/read-string (pr-str (read))))

(defn modal [f & args]
  (fn [_]
    (apply f args) 
    (set-mode! :normal)))

(defn mode-setter [mode]
  (fn [_] (set-mode! mode)))

(defn multi
  ([f]
   (multi e/edit f))
  ([target f] 
   (fn [_] (mult-cmd target f))))

(defn once
  ([f]
   (once e/edit f))
  ([target f] 
   (fn [_] (target f))))

(defn once-input [f]
  (fn [_] (e/edit f (input))))

(def quit                     (constantly :quit))
(def redraw                   (fn [_] (pprint)))
(def read-file                (fn [_] (e/read-file (str (read)))))

(def normal-mode              (mode-setter :normal))
(def undo-mode                (mode-setter :undo))
(def delete-mode              (mode-setter :delete))

(def move-left                (multi c/move-left))
(def move-leftmost            (once c/move-leftmost))
(def move-right               (multi c/move-right))
(def move-rightmost           (once c/move-rightmost))
(def move-up                  (multi c/move-up))
(def move-down                (multi c/move-down))
(def move-next                (multi c/move-next))
(def move-prev                (multi c/move-prev))

(def insert-leftmost-child    (once-input c/insert-leftmost-child))
(def insert-left              (once-input c/insert-left))
(def insert-right             (once-input c/insert-right))
(def insert-leftmost          (once-input c/insert-leftmost))
(def insert-rightmost         (once-input c/insert-rightmost))
(def insert-break-left        (multi c/insert-break-left))
(def insert-break-right       (multi c/insert-break-right))

(def replace-point            (once-input c/replace-point))

(def delete-point             (multi c/delete-point))
(def delete-left              (multi c/delete-left))
(def delete-lefts             (once c/delete-lefts))
(def delete-right             (multi c/delete-right))
(def delete-rights            (once c/delete-rights))
(def delete-siblings          (once c/delete-siblings))

(def undo-left                (multi e/undo c/move-left))
(def undo-leftmost            (multi e/undo c/move-leftmost))
(def undo-right               (multi e/undo c/move-right))
(def undo-rightmost           (multi e/undo c/move-rightmost))
(def undo-up                  (multi e/undo c/move-up))
(def undo-down                (multi e/undo c/move-down))
(def undo-next                (multi e/undo c/move-next))
(def undo-prev                (multi e/undo c/move-prev))

(def key-bindings
  (atom {:normal
         {:dispatch mult-dispatch
          \formfeed redraw
          \q        quit
          \u        undo-mode
          \d        delete-mode
          \h        move-left
          \^        move-leftmost
          \l        move-right
          \$        move-rightmost
          \j        move-down
          \k        move-up
          \L        move-next
          \H        move-prev
          \x        delete-point
          \e        read-file
          \c        replace-point
          \C        insert-leftmost-child
          \i        insert-left
          \I        insert-leftmost
          \a        insert-right
          \A        insert-rightmost
          \O        insert-break-left
          \o        insert-break-right}
         :delete
         {:dispatch mult-dispatch
          \d        (modal delete-siblings) 
          \h        (modal delete-left)
          \l        (modal delete-right)}
         :undo
         {:dispatch mult-dispatch
          \h        undo-left
          \^        undo-leftmost
          \l        undo-right
          \$        undo-rightmost
          \j        undo-down
          \k        undo-up
          \L        undo-next
          \H        undo-prev}}))

(defn status-line [text]
  (subs (apply str text (repeat cols "-")) 0 cols))

(defn status []
  (println (status-line (format "[%s] [%s]" (str @mode) @e/file))))

(defn pprint []
  (print "\033[2J\r")
  (let [colr  (fn [x] [:span [:pass (s/cursor x)] x])
        pnt   (zip/node (zip/node @@e/point))
        post  #(if (::point (meta %1)) (colr %2) %2)
        src   (with-out-str
                (binding [pp/post-process post]
                  (-> (zip/node @@e/point)
                    (zip/edit vary-meta assoc ::point true)
                    s/mark-syntax
                    (s/mark-point ::point)
                    (s/colorize ::point :point)
                    zip/root
                    pp/pprint)))
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
        (do (set-mode! :normal) (recur))
        (let [k ((get-in @key-bindings [@mode :dispatch]) c) 
              f (get-in @key-bindings [@mode k])]
          (if (or (not f) (not= :quit (f c))) (recur)))))))

