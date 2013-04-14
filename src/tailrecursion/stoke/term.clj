(ns tailrecursion.stoke.term
  (:import (java.io PushbackReader))
  (:require
    [clojure.java.io                  :as io]
    [clojure.zip                      :as zip]
    [clojure.string                   :as string]
    [clojure.java.shell               :as shell]
    [lanterna.terminal                :as t]
    [tailrecursion.stoke.edit         :as e]
    [tailrecursion.stoke.read         :as r]
    [tailrecursion.stoke.print        :as pp]
    [tailrecursion.stoke.util         :as u]
    [tailrecursion.stoke.cmd          :as c]
    [tailrecursion.stoke.syntax       :as s]
    [tailrecursion.stoke.term.colors  :refer [colors]]))

(declare key-bindings pprint)

(def parseInt #(Integer/parseInt %))

(def lines  ((fnil parseInt "80") (System/getenv "LINES"))) 
(def cols   ((fnil parseInt "25") (System/getenv "COLUMNS"))) 
(def mode   (atom :normal))
(def mult   (atom nil))
(def buffer (atom [""]))
(def cmd    (atom ""))

(defn set-mode! [x]
  (when (get @key-bindings x)
    (reset! mult nil) 
    (reset! mode x)
    (reset! cmd "")
    (swap! @e/point identity))
  x)

(defn update-mult! [c]
  (swap! mult #(let [i (Character/digit c 10)]
                 (if (not %) i (+ (* 10 %) i)))))

(defn mult-dispatch [c]
  (if (and (char? c) (Character/isDigit c)) (update-mult! c) c))

(defn mult-cmd [g f & args]
  (dotimes [i (or @mult 1)] (apply g f args))
  (reset! mult nil))

(defn input []
  (r/read-string (pr-str (read))))

(defn modal [f]
  (fn [x]
    (f x) 
    (set-mode! :normal)))

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

(defn enter-mode [mode f]
  (fn [c]
    (set-mode! mode)
    (f c)))

(def key-bindings (atom {}))

(defn status-line [& texts]
  (let [text (string/join
               (str \u2500 \u2500)
               (cons "" (map #(format " %s " %) texts)))
        line (subs (apply str text (repeat cols (str \u2500))) 0 cols)] 
    (str "\033[38;5;230m\033[1m" line "\033[0m")))

(defn status []
  (let [mode (str @mode)
        file (if (string? @e/file)
               (.getName (io/file @e/file))
               (str @e/file))]
    (status-line mode file)))

(defn center [lines]
  (let [lnlen (count (str (count lines)))
        shift (- (int (Math/floor (/ (- cols pp/width) 2))) (inc lnlen)) 
        pad   (apply str (repeat shift " "))]
    (map #(str pad %) lines)))

(defn pprint [point]
  (let [colr  (fn [x] [:span [:pass (s/cursor x)] x])
        post  #(if (::point (meta %1)) (colr %2) %2)
        rmbr  #(-> %
                 (string/replace #" \x08\n\n" "\n")
                 (string/replace #" \x08" "")) 
        src   (rmbr 
                (with-out-str
                  (binding [pp/post-process post]
                    (-> (zip/node point)
                      (zip/edit vary-meta assoc ::point true)
                      s/mark-syntax
                      (s/mark-point ::point)
                      (s/colorize ::point :point)
                      zip/root
                      pp/pprint)))) 
        [x y] (let [s (string/split src #"\n")
                    l (count (str (count s)))
                    c (get-in colors [:line-nums :color])
                    f (format "\033[38;5;%dm%%%dd\033[0m %%s" c l)] 
                (->> s
                  (map-indexed #(format f (inc %1) %2))
                  (split-with #(not (re-find #"\f" %))))) 
        nx    (count x)
        ny    (count y)
        xtra  (/ (- lines (+ nx ny)) 2)
        pad   " "
        padt  (repeat (int (Math/floor xtra)) pad)
        padb  (repeat (int (Math/ceil xtra)) pad)
        padx  (repeat (- ny nx) pad)
        pady  (repeat (- nx ny) pad)
        all   (concat padt padx x y pady padb)
        nw    (count all)
        over  (int (Math/ceil (/ (- nw lines) 2)))
        win   (->> (if (< 0 over) (drop over all) all) (take lines))]
    (conj (vec (drop-last 2 (center win))) (status) @cmd)))

(defn paint [term output-lines]
  (doall
    (map-indexed
      #(let [b (try (nth @buffer %1) (catch Throwable e))
             c (apply str (repeat cols " "))]
         (if (not= b %2) (t/put-string term (str c "\r" %2) 0 %1)))
      output-lines))
  (t/move-cursor term 0 lines)
  (reset! buffer (vec output-lines)))

(defn repaint [_]
  (reset! buffer [])
  (swap! @e/point identity))

(defn read-loop []
  (loop []
    (let [c (char (.read System/in))]
      (let [k ((get-in @key-bindings [@mode :dispatch]) c) 
            f ((get-in @key-bindings [@mode k] identity) c)]
        (if (cond (= :quit f) false (keyword? f) (set-mode! f) :else true)
          (recur))))))

(let [term (t/get-terminal :text)
      work (Thread. read-loop)]

  (add-watch cmd ::command (fn [_ _ _ _]
                             (paint term (concat (butlast @buffer) [@cmd]))))

  (defn read-file [f]
    (e/read-file f #(paint term (pprint %)))) 

  (defn start-loop [f]
    (t/in-terminal
      term
      (e/init #(paint term (pprint %)))
      (.start work)
      (.join work)))) 

