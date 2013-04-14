(ns tailrecursion.stoke.edit
  (:require
    [clojure.java.io            :as io]
    [clojure.zip                :as zip]
    [tailrecursion.stoke.read   :as r]
    [tailrecursion.stoke.print  :as p]
    [tailrecursion.stoke.util   :as u]))

(defn ok? [x]
  (if (and x (zip/node x))
    (let [n (zip/node x)
          nn (zip/node n)
          nr (zip/root n)]
      (and (not= nr nn) (seq nr)))))

(defn scratch []
  (-> (atom "Ready.") r/read-all))

(defn start-zip [forms]
  (-> forms r/zipper zip/down))

(defn make-point []
  (atom (-> (scratch) start-zip u/meta-zip) :validator ok?))

(def point (atom (make-point) :validator (complement nil?)))
(def file  (atom :scratch))
(def files (atom {@file @point}))

(defn get-point [] (-> @@point zip/node zip/node))

(defn add-history [x]
  (swap! @point #(zip/rightmost (zip/down (zip/append-child % x)))))

(defn read-file [f watch-fn]
  (if-let [p (get @files f)] 
    (do
      (reset! point p)
      (reset! file f))
    (let [forms (if (.exists (io/file f)) (r/read-file f) (scratch))]
      (reset! point (make-point)) 
      (add-watch @point ::read-file (fn [_ _ _ x] (watch-fn x)))
      (reset! file f) 
      (swap! files assoc f @point) 
      (add-history (start-zip forms)))))

(defn write-file []
  (binding [p/fanciness false]
    (try
      (->>
        (with-out-str (-> @@point zip/node zip/root p/pprint))
        (spit @file)) 
      true
      (catch Throwable e false))))

(defn edit [f & args]
  (try
    (let [prv (zip/root (zip/node @@point))
          p   (apply f (zip/node @@point) args)
          nxt (zip/root p)]
      (if (= prv nxt)
        (swap! @point zip/replace p)
        (add-history p)))
    (catch Throwable e)))

(defn undo [f & args]
  (try
    (apply swap! @point f args)
    (catch Throwable e)))

(defn init [watch-fn]
  (watch-fn @@point)
  (add-watch @point ::read-file (fn [_ _ _ x] (watch-fn x))))
