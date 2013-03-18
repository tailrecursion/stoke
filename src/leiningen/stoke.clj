(ns leiningen.stoke
  (:require
    [clojure.zip        :as z]
    [clojure.string     :as string]
    [clojure.pprint     :as pp]
    [lanterna.screen    :as s]
    [lanterna.terminal  :as t]))

(def point (atom nil))
(def file "../javelin/src/cljs/tailrecursion/javelin/core.cljs")

(defmulti stoke-dispatch
  (fn [x] (boolean (identical? (z/node @point) x))))

(defmethod stoke-dispatch true [thing]
  (print "\u001B[35m")
  (pp/code-dispatch thing)
  (print "\u001B[0m"))

(defmethod stoke-dispatch :default [thing]
  (pp/code-dispatch thing))

(defn cls [scr]
  (let [[cols rows] (s/get-size scr)
        blank       (format (format "%%%ds" cols) " ")]
    (doall (map #(s/put-string scr 0 % blank) (range 0 rows)))))

(defn make-printer [scr]
  (fn [src]
    (let [draw-line #(s/put-string scr 0 %1 %2)]
      (doall (map-indexed draw-line (string/split src #"\n"))))))

(defn draw [scr]
  (pp/with-pprint-dispatch
    stoke-dispatch
    ((make-printer scr) (with-out-str (pp/pprint (z/root @point))))))

(defn stoke
  "I don't do a lot."
  [project & args]
  (pp/with-pprint-dispatch
    stoke-dispatch
    (let [scr   (s/get-screen :unix)
          frm   (z/seq-zip (read-string (str "(" (slurp file) ")")))
          prt   (make-printer scr)]
      (reset! point (z/right (z/down frm)))
      (s/in-screen
        scr
        (loop []
          (cls scr)
          (s/redraw scr)
          (draw scr) 
          (s/redraw scr)
          (if (case (s/get-key-blocking scr)
                \q nil
                \l (if (seq (z/rights @point)) (reset! point (z/right @point))) 
                \h (if (seq (z/lefts @point)) (reset! point (z/left @point))) 
                \j (reset! point ((fnil identity @point) (z/down @point)))
                \k (reset! point ((fnil identity @point) (z/up @point)))
                ::ok)
            (recur)))))))

(comment

  (code-str file)
  
  )
