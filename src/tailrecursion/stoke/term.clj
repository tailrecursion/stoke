(ns tailrecursion.stoke.term
  (:require
    [clojure.java.io            :as io]
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

(defn modal [f]
  (fn [x]
    (f x) 
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

(def placeholder (str \u2588))

(defn paredit-mode [op]
  (set-mode! :paredit)
  (e/edit op (r/read-string placeholder)))

(defn paredit-mode-left [_]
  (paredit-mode c/insert-left))

(defn paredit-mode-leftmost [_]
  (paredit-mode c/insert-leftmost))

(defn paredit-mode-right [_]
  (paredit-mode c/insert-right))

(defn paredit-mode-rightmost [_]
  (paredit-mode c/insert-rightmost))

(defn paredit-mode-rightmost-child [_]
  (paredit-mode c/insert-rightmost-child))

(defn paredit-mode-replace [_]
  (paredit-mode c/replace-point))

(defn paredit-insert-seq [src]
  (let [op (if (= placeholder (str (e/get-point)))
             c/replace-point
             c/insert-right)] 
    (e/edit op (r/read-string src)) 
    (e/edit c/insert-rightmost-child (r/read-string placeholder))))

(defn paredit-insert-expr [_]
  (paredit-insert-seq "()"))

(defn paredit-insert-vec [_]
  (paredit-insert-seq "[]"))

(defn paredit-edit-sym [c]
  (let [p (str (e/get-point))]
    (e/edit c/replace-point (r/read-string (if (= placeholder p)
                                             (str c)
                                             (str p c))))))

(defn paredit-next [_]
  (e/edit c/insert-right (r/read-string placeholder)))

(defn paredit-up [_]
  (e/edit c/move-up)
  (e/edit c/insert-right (r/read-string placeholder)))

(defn paredit-dispatch [c]
  (if (re-find r/re-scalar (str c)) \a c))

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
          \c        paredit-mode-replace
          \C        paredit-mode-rightmost-child
          \i        paredit-mode-left
          \I        paredit-mode-leftmost
          \a        paredit-mode-right
          \A        paredit-mode-rightmost
          \O        insert-break-left
          \o        insert-break-right}
         :delete
         {:dispatch mult-dispatch
          (char 27) normal-mode
          \d        (modal delete-siblings) 
          \h        (modal delete-left)
          \l        (modal delete-right)}
         :undo
         {:dispatch mult-dispatch
          (char 27) normal-mode
          \h        undo-left
          \^        undo-leftmost
          \l        undo-right
          \$        undo-rightmost
          \j        undo-down
          \k        undo-up
          \L        undo-next
          \H        undo-prev}
         :paredit
         {:dispatch paredit-dispatch
          (char 27) normal-mode
          \newline  normal-mode
          \(        paredit-insert-expr
          \)        paredit-up
          \[        paredit-insert-vec
          \]        paredit-up
          \a        paredit-edit-sym
          \space    paredit-next
          }
         }))

(defn status-line [& texts]
  (let [text (string/join
               (str \u2500 \u2500)
               (cons "" (map #(format " %s " %) texts)))
        line (subs (apply str text (repeat cols (str \u2500))) 0 cols)] 
    (str "\033[38;5;230m\033[1m" line "\033[0m")))

(defn status []
  (let [mode (str @mode)
        file (.getName (io/file @e/file))]
    (println (status-line mode file))))

(defn pprint []
  (print "\033[2J\r")
  (let [colr  (fn [x] [:span [:pass (s/cursor x)] x])
        pnt   (zip/node (zip/node @@e/point))
        post  #(if (::point (meta %1)) (colr %2) %2)
        rmbr  #(string/replace % #" \x08\n\n" "\n")
        src   (rmbr 
                (with-out-str
                  (binding [pp/post-process post]
                    (-> (zip/node @@e/point)
                      (zip/edit vary-meta assoc ::point true)
                      s/mark-syntax
                      (s/mark-point ::point)
                      (s/colorize ::point :point)
                      zip/root
                      pp/pprint)))) 
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
      (let [k ((get-in @key-bindings [@mode :dispatch]) c) 
            f (get-in @key-bindings [@mode k])]
        (if (or (not f) (not= :quit (f c))) (recur))))))

