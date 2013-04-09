(ns tailrecursion.stoke.mode.paredit
  (:require
    [clojure.zip                      :as zip]
    [tailrecursion.stoke.edit         :as e]
    [tailrecursion.stoke.read         :as r]
    [tailrecursion.stoke.cmd          :as c]
    [tailrecursion.stoke.print        :as p]))

(def placeholder (str \u2588))

(defn trim [s n]
  (apply str (drop-last n s)))

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

(defn paredit-insert-map [_]
  (paredit-insert-seq "{}"))

(defn paredit-edit-sym [c]
  (let [p (str (e/get-point))
        s (if (= placeholder p) (str c) (str p c))]
    (e/edit c/replace-point (r/read-string s))))

(defn paredit-next [_]
  (e/edit c/insert-right (r/read-string placeholder)))

(defn paredit-up [_]
  (e/edit c/move-up)
  (e/edit c/insert-right (r/read-string placeholder)))

(defn dispatch-char [point type c]
  (if (and (= "\\" (str point))
           (contains? (set (mapcat identity r/delims)) c))
    (paredit-edit-sym c)))

(defn dispatch-str [point type c]
  (cond
    (and (= placeholder (str point)) (= \" c))
    (e/edit c/replace-point (r/read-string "\"\""))
    (= :str type)
    (let [s (subs (trim (first point) 1) 1)]
      (cond
        (= (char 127) c)
        (cond
          (= \\ (last (trim s 1)))
          (e/edit c/replace-point (r/read-string (str \" (trim s 2) \")))
          (< 0 (count s))
          (e/edit c/replace-point (r/read-string (str \" (trim s 1) \")))
          :else
          (e/edit c/replace-point (r/read-string placeholder)))
        (= \u25A1 (last s))
        (e/edit c/replace-point (r/read-string (str \" (trim s 1) c \")))
        (= \\ c)
        (e/edit c/replace-point (r/read-string (str \" s c \u25A1 \")))
        (= \" c)
        (e/edit c/insert-right (r/read-string placeholder))
        :else
        (e/edit c/replace-point (r/read-string (str \" s c \")))))))

(defn paredit-dispatch [c]
  (let [p (e/get-point)
        t (p/type* p)]
    (dispatch-str p t c)))

