(ns tailrecursion.stoke.edit
  (:require
    [clojure.zip                :as zip]
    [tailrecursion.stoke.reader :as r]))

(defn meta-zip [root]
  (let [branch?   #(instance? clojure.lang.IMeta %)
        children  #(get (meta %) ::children)
        make-node #(vary-meta %1 assoc ::children %2)]
    (zip/zipper branch? children make-node root)))

(let [ok?   #(not (or (nil? %) (nil? (zip/node %))))
      init  (meta-zip (zip/vector-zip []))]
  (def point (atom init :validator ok?)))

(defn add-history [x]
  (swap! point #(zip/rightmost (zip/down (zip/append-child % x)))))

(defn read-file [f]
  (add-history (zip/down (r/zipper (r/read-file f)))))

(defn edit [f & args]
  (try
    (let [prv (zip/root (zip/node @point))
          p   (apply f (zip/node @point) args)
          nxt (zip/root p)]
      (if (= prv nxt)
        (swap! point zip/replace p)
        (add-history p)))
    (catch Throwable e)))

(defn undo [f & args]
  (try
    (apply swap! point f args)
    (catch Throwable e)))

