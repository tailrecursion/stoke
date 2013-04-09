(ns tailrecursion.stoke.mode.paredit
  (:require
    [clojure.zip                      :as zip]
    [tailrecursion.stoke.edit         :as e]
    [tailrecursion.stoke.read         :as r]
    [tailrecursion.stoke.cmd          :as c]))

(def placeholder (str \u2588))

(defn paredit-mode [op]
  (e/edit op (r/read-string placeholder)))

(defn paredit-mode-left [_]
  (paredit-mode c/insert-left))

(defn paredit-mode-leftmost [_]
  (paredit-mode c/insert-leftmost))

(defn paredit-mode-right [_]
  (paredit-mode c/insert-right))

(defn paredit-mode-rightmost [_]
  (paredit-mode c/insert-rightmost))

(defn paredit-mode-rightmost-child [_]
  (paredit-mode c/insert-rightmost-child))

(defn paredit-mode-replace [_]
  (paredit-mode c/replace-point))

(defn paredit-insert-seq [src]
  (let [op (if (= placeholder (str (e/get-point)))
             c/replace-point
             c/insert-right)] 
    (e/edit op (r/read-string src)) 
    (e/edit c/insert-rightmost-child (r/read-string placeholder))))

(defn paredit-insert-expr [_]
  (paredit-insert-seq "()"))

(defn paredit-insert-vec [_]
  (paredit-insert-seq "[]"))

(defn paredit-edit-sym [c]
  (let [p (str (e/get-point))]
    (e/edit c/replace-point (r/read-string (if (= placeholder p)
                                             (str c)
                                             (str p c))))))

(defn paredit-next [_]
  (e/edit c/insert-right (r/read-string placeholder)))

(defn paredit-up [_]
  (e/edit c/move-up)
  (e/edit c/insert-right (r/read-string placeholder)))

(defn paredit-dispatch [c]
  (if (re-find r/re-scalar (str c)) \a c))

