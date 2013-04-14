(ns tailrecursion.stoke.syntax
  (:require
    [clojure.zip                      :as zip]
    [clojure.string                   :as string]
    [tailrecursion.stoke.print        :as pp]
    [tailrecursion.stoke.read         :as r]
    [tailrecursion.stoke.util         :as u]
    [tailrecursion.stoke.term.colors  :refer [colors]]))

(defn cursor [x]
  (let [c (if (= x :break) (str \u2588) "")]
    (format "\033[38;5;%dm\f%s\033[0m" (get-in colors [:point :color]) c)))

(defn set-color [x k]
  (vary-meta x merge (get colors k)))

(defn package [x]
  (try
    (.. (type (resolve x)) getPackage getName)
    (catch Throwable e)))

(defn in-package? [x pkg]
  (if-let [xpkg (package x)]
    (let [x (string/split xpkg #"\.")
          y (string/split pkg #"\.")
          n (min (count x) (count y))]
      (and (< 0 n) (= (take n x) (take n y))))))

(defn keyword?* [x] (and (= :sym (pp/type* x)) (= \: (first (str x)))))
(defn symbol?*  [x] (and (= :sym (pp/type* x)) (not= \: (first (str x)))))
(defn core?*    [x] (and (= :sym (pp/type* x)) (in-package? x "clojure")))
(defn java?*    [x] (and (= :sym (pp/type* x)) (in-package? x "java")))
(defn string?*  [x] (= :str (pp/type* x)))
(defn char?*    [x] (and (= :sym (pp/type* x)) (= \\ (first (str x)))))
(defn meta?*    [x] (= (symbol "^") (:prefix (meta x))))
(defn defvar?*  [z] (let [n (zip/node z)]
                      (if (and (symbol?* n) (not (meta?* n))) 
                        (let [zz (u/repeat-while z meta?* zip/left)
                              n  (if zz (zip/node zz))]
                          (or (= 'def n) (= 'defn n) (= 'defn- n))))))

(defn dfwalk [z f & args]
  (apply u/depth-first-walk z r/zipper f args))

(defn mark-point [z k]
  (dfwalk z #(let [k? (fn [x] (get (meta x) k))
                   p? (some k? (cons (zip/node %) (zip/path %)))] 
               (try
                 (zip/edit % vary-meta assoc k p?)
                 (catch Throwable e %)))))

(defn mark-syntax [z]
  (dfwalk z #(let [n  (zip/node %)
                   k  (cond (defvar?* %)   :defvar
                            (char?* n)     :char
                            (keyword?* n)  :keyword
                            (core?* n)     :clj-core
                            (java?* n)     :java-cls
                            (symbol?* n)   :symbol
                            (string?* n)   :string
                            :else          :default)] 
               (try
                 (zip/edit % set-color k)
                 (catch Throwable e %)))))

(defn colorize [z k color]
  (dfwalk z #(try
               (if (get (meta (zip/node %)) k)
                 (zip/edit % set-color color) %) 
               (catch Throwable e %))))
