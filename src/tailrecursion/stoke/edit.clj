(ns tailrecursion.stoke.edit
  (:require
    [clojure.java.io          :as io]
    [clojure.zip              :as zip]
    [tailrecursion.stoke.read :as r]))

(defn meta-zip [root]
  (let [branch?   #(instance? clojure.lang.IMeta %)
        children  #(get (meta %) ::children)
        make-node #(vary-meta %1 assoc ::children %2)]
    (zip/zipper branch? children make-node root)))

(defn make-point []
  (let [ok? #(not (or (nil? %) (nil? (zip/node %))))]
    (atom (meta-zip (zip/vector-zip [])) :validator ok?))) 

(def point (atom (make-point) :validator (complement nil?)))
(def file  (atom ""))
(def files (atom {@file @point}))

(defn add-history [x]
  (swap! @point #(zip/rightmost (zip/down (zip/append-child % x)))))

(defn read-file [f]
  (if-let [p (get @files f)] 
    (do
      (reset! point p)
      (reset! file f))
    (when (.exists (io/file f))
      (reset! point (make-point)) 
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

