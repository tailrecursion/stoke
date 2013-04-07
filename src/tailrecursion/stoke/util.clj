(ns tailrecursion.stoke.util
  (:require
    [clojure.zip :as zip]))

(declare remove-children)

(defn meta-zip [root]
  (let [branch?   #(instance? clojure.lang.IMeta %)
        children  #(get (meta %) ::children)
        make-node #(vary-meta %1 assoc ::children %2)]
    (zip/zipper branch? children make-node root)))

(defn zip-root [loc]
  (if (= :end (loc 1))
    loc
    (let [p (zip/up loc)]
      (if p
        (recur p)
        loc))))

(defn set-mark [z]
  (zip/edit z vary-meta assoc ::mark true))

(defn get-mark [z]
  (loop [loc (zip-root z)]
    (if (or (::mark (meta (zip/node loc)))
            (zip/end? loc)) 
      (zip/edit loc vary-meta dissoc ::mark) 
      (recur (zip/next loc)))))

(defn repeat-while [z pred? f & args]
  (loop [loc (apply f z args)]
    (if-let [nxt (and (pred? (zip/node loc)) (apply f loc args))] 
      (recur nxt)
      loc)))

(defn guard [z guarded f & args]
  (if-let [loc (guarded z)]
    (apply f loc args)
    z))

(defn insert-child [z x]
  (-> (zip/insert-child z x) zip/down))

(defn append-child [z x]
  (-> (zip/append-child z x) zip/down zip/rightmost))

(defn insert-left [z x]
  (-> (zip/insert-left z x) zip/left))

(defn insert-right [z x]
  (-> (zip/insert-right z x) zip/right))

(defn insert-rightmost [z x]
  (-> (zip/up z) (zip/append-child x) zip/down zip/rightmost))

(defn insert-leftmost [z x]
  (-> (zip/up z) (zip/insert-child x) zip/down))

(defn remove-left [z]
  (-> (set-mark z) zip/left zip/remove get-mark))

(defn remove-right [z]
  (-> (set-mark z) zip/right zip/remove get-mark))

(defn remove-point [z]
  (cond
    (zip/right z) (-> (zip/right z) set-mark zip/left zip/remove get-mark)
    (zip/left z)  (-> (zip/left z) set-mark zip/right zip/remove get-mark)
    :else         (zip/remove z)))

(defn remove-parent [z]
  (-> (zip/up z) zip/remove))

(defn remove-rights [z]
  (-> (repeat-while zip/right remove-right) remove-point))

(defn remove-lefts [z]
  (-> (repeat-while zip/left remove-left) remove-point))

(defn remove-siblings [z]
  (-> (zip/up z) remove-children))

(defn remove-leftmost-child [z]
  (-> (set-mark z) zip/down zip/remove get-mark))

(defn remove-rightmost-child [z]
  (-> (set-mark z) zip/down zip/rightmost zip/remove get-mark))

(defn remove-children [z]
  (-> (repeat-while zip/down remove-leftmost-child)))

(defn replace-point [z x]
  (zip/replace z x))

(defn replace-left [z x]
  (guard z zip/left zip/replace x))

(defn replace-right [z x]
  (guard z zip/right zip/replace x))

