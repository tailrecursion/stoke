(ns tailrecursion.stoke.zip
  (:require
    [clojure.zip    :as z]
    )
  )

(def point (atom nil))

(defn- seq->map [seq] (apply hash-map (mapcat identity seq)))
(defn- seq->vec [seq] (apply vector seq))
(defn- seq->set [seq] (set seq))

(defn zip [root]
  (z/zipper )
  )

(comment

  (require '[clojure.pprint :as pp])

  (set (seq #{1 2 3 4}))

  (binding [*print-meta* true]
    (pp/pprint (read-string "#(def ^:export foo 77)"))) 
  (binding [pp/*print-pprint-dispatch* pp/code-dispatch]
    (pp/pprint
      (apply hash-map (mapcat identity (seq {1 2 3 4}))))) 

  )
