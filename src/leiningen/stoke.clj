(ns leiningen.stoke
  (:require
    [clojure.string     :as string]
    [clojure.pprint     :as pp]
    [lanterna.screen    :as s]
    [lanterna.terminal  :as t]))

(def scr    (atom nil))
(def src    (atom ""))
(def exp    (atom [[0 0] [0 0]]))

(def file "../javelin/src/cljs/tailrecursion/javelin/core.cljs")

(defn pp-form [form]
  (binding [*print-meta*                true
            pp/*print-pprint-dispatch*  pp/code-dispatch]
    (with-out-str (pp/pprint form))))

(def ^:dynamic *pprinter* pp-form)

(defn code-str [f]
  (->> (str "(" (slurp f) ")")
    read-string
    (mapv *pprinter*)
    (string/join "\n")))

(defn lines [s] (string/split s #"\n"))

(defn safe-read-string [s]
  (try (read-string s)
    (catch Throwable e ::nope)))

(defn cur-exp-str []
  (let [lns   (string/join "\n" (drop (get-in @exp [0 1]) (lines @src))) 
        src   (subs lns (get-in @exp [0 0]))
        chr   (first src)]
    (loop [n 0, prev ""]
      (if (< n (count src))
        (let [f (subs src 0 n) 
              x (safe-read-string f)
              y (safe-read-string prev)]
          (if (< (count (string/trim f)) 50)
            (println ">>>" f "<<<")) 
          (if (or (= ::nope x) (not= x y)) (recur (inc n) f) prev))
        (do (s/clear @scr) (s/redraw @scr) (throw (Exception. (str "unreadable: " src))))))))

(defn update-exp! []
  (let [cur (lines (cur-exp-str))
        pad (if (= 1 (count cur)) (get-in @exp [0 0]) 0)
        x   (+ pad (count (last cur))) 
        y   (+ (get-in @exp [0 1]) (dec (count cur)))]
    (swap! exp assoc 1 [x y])))

(defn draw [& _]
  (update-exp!)
  (s/clear @scr)
  (let [src (lines @src)
        [[x1 y1] [x2 y2]] @exp]
    (loop [x 0, y 0]
      (let [color (cond (or (and (= y1 y2 y) (<= x1 x) (> x2 x))
                            (and (= y1 y) (not= y1 y2) (<= x1 x))
                            (and (= y2 y) (not= y1 y2) (> x2 x))
                            (and (< y1 y) (> y2 y))) [{:fg :black :bg :blue}]
                        :else [])
            line  (try (nth src y) (catch Throwable e)) 
            len   (count line)]
        (if (< x len) (apply s/put-string @scr x y (subs line x (inc x)) color))
        (cond (< x (dec len))   (recur (inc x) y)
              (< y (count src)) (recur 0 (inc y))))))
  (s/redraw @scr)
  ::ok)

(defn right! []
  (let [[x2 y2] (nth @exp 1)
        lns     (drop y2 (lines @src))]
    (cond (and (>= (inc x2) (count (first lns))) (< 1 (count lns))) 
          (swap! exp assoc 0 [0 (inc y2)])
          (< (inc x2) (count (first lns)))
          (swap! exp assoc 0 [(inc x2) y2]))
    (draw)))

(defn stoke
  "I don't do a lot."
  [project & args]
  (reset! scr   (s/get-screen :unix {:resize-listener draw})) 
  (reset! src   (code-str file))
  (s/in-screen
    @scr
    (draw)
    (loop []
      (if (case (s/get-key-blocking @scr)
            \q nil
            \l (right!)
            (draw))
        (recur)))))

(comment

  (code-str file)
  
  )
