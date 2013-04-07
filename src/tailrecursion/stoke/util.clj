(ns tailrecursion.stoke.util
  (:require
    [clojure.zip :as zip]))

(defn zip-root [loc]
  (if (= :end (loc 1))
    loc
    (let [p (zip/up loc)]
      (if p
        (recur p)
        loc))))

(defn meta-zip [root]
  (let [branch?   #(instance? clojure.lang.IMeta %)
        children  #(get (meta %) ::children)
        make-node #(vary-meta %1 assoc ::children %2)]
    (zip/zipper branch? children make-node root)))

(defn set-mark [z]
  (zip/edit z vary-meta assoc ::mark true))

(defn get-mark [z]
  (loop [loc (zip-root z)]
    (if (or (::mark (meta (zip/node loc)))
            (zip/end? loc)) 
      (zip/edit loc vary-meta dissoc ::mark) 
      (recur (zip/next loc)))))

