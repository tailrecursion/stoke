(ns tailrecursion.stoke.screen
  (:require
    [tailrecursion.stoke.edit :as e :refer [point]]
    [clojure.string           :as string]
    [clojure.zip              :as z]
    [clojure.pprint           :as pp]
    [lanterna.screen          :as s]
    [lanterna.terminal        :as t]))

(def scroll-top   (atom 0))
(def cmd-rows     (atom 1))

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
    (let [draw-line     #(s/put-string scr 0 %1 %2)
          [cols rows]   (s/get-size scr)
          lines         (string/split src #"\n")
          visible-lines (take (- rows @cmd-rows) (drop @scroll-top lines))]
      (doall (map-indexed draw-line visible-lines))
      (draw-line (dec rows) "comand> (fuck-off you-idiot)")
      )))

(defn draw [scr]
  (pp/with-pprint-dispatch
    stoke-dispatch
    ((make-printer scr) (with-out-str (pp/pprint (z/root @point))))))

(defn start [command]
  (let [scr (s/get-screen :unix)]
    (s/in-screen
      scr
      (loop []
        (cls scr)
        (s/redraw scr)
        (draw scr) 
        (s/redraw scr)
        (if (not= :quit (command (s/get-key-blocking scr))) 
          (recur))))))
