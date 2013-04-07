(ns tailrecursion.stoke.syntax
  (:require
    [clojure.pprint             :refer [pprint]]
    [clojure.zip                :as zip]
    [tailrecursion.stoke.print  :as pp]
    [tailrecursion.stoke.read   :as r]
    [tailrecursion.stoke.util   :as u]))

(defn mark-point [src-zip k]
  (loop [loc (r/zipper (zip/root src-zip))]
    (let [p? (some #(get (meta %) k) (cons (zip/node loc) (zip/path loc)))
          p  (try
               (zip/edit loc vary-meta assoc k p?)
               (catch Throwable e loc))] 
      (if (zip/end? p) p (recur (zip/next p))))))

(defn colorize [src-zip k color]
  (loop [loc (r/zipper (zip/root src-zip))]
    (let [p? (get (meta (zip/node loc)) k)
          p  (try
               (zip/edit loc vary-meta assoc :color (if p? color 103))
               (catch Throwable e loc))]
      (if (zip/end? p) p (recur (zip/next p))))))
