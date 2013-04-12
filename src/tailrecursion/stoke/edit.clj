(ns tailrecursion.stoke.edit
  (:require
    [clojure.java.io          :as io]
    [clojure.zip              :as zip]
    [tailrecursion.stoke.read :as r]
    [tailrecursion.stoke.util :as u]))

(defn make-point []
  (let [ok? #(not (or (nil? %) (nil? (zip/node %)) (= (zip/root (zip/node %)) (zip/node (zip/node %)))))]
    (atom (-> (atom "(comment welcome to stoke!)")
            r/read-all
            r/zipper
            zip/down
            u/meta-zip)
          :validator ok?)))

(def point (atom (make-point) :validator (complement nil?)))
(def file  (atom ""))
(def files (atom {@file @point}))

(defn get-point [] (-> @@point zip/node zip/node))

(defn add-history [x]
  (swap! @point #(zip/rightmost (zip/down (zip/append-child % x)))))

(defn read-file [f watch-fn]
  (if-let [p (get @files f)] 
    (do
      (reset! point p)
      (reset! file f))
    (when (.exists (io/file f))
      (reset! point (make-point)) 
      (add-watch @point ::read-file (fn [_ _ _ x] (watch-fn x)))
      (reset! file f) 
      (swap! files assoc f @point) 
      (add-history (zip/down (r/zipper (r/read-file f)))))))

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

