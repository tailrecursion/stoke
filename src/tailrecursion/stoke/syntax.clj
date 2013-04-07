(ns tailrecursion.stoke.syntax
  (:require
    [clojure.pprint             :refer [pprint]]
    [clojure.string             :as string]
    [clojure.zip                :as zip]
    [tailrecursion.stoke.print  :as pp]
    [tailrecursion.stoke.read   :as r]
    [tailrecursion.stoke.util   :as u]))

(def colors
  {:default   {:color 103 :bold 0}
   :point     {:color 214 :bold 1}
   :keyword   {:color 107 :bold 0}
   :symbol    {:color 103 :bold 0}
   :clj-core  {:color 103 :bold 1}
   :java-cls  {:color 203 :bold 1}
   :string    {:color  14 :bold 0}
   })

(defn cursor [x]
  (let [c (if (= x :break) (str \u2588) "")]
    (format "\033[38;5;%dm\f%s\033[0m" (get-in colors [:point :color]) c)))

(defn mark-point [src-zip k]
  (loop [loc (r/zipper (zip/root src-zip))]
    (let [p? (some #(get (meta %) k) (cons (zip/node loc) (zip/path loc)))
          p  (try
               (zip/edit loc vary-meta assoc k p?)
               (catch Throwable e loc))] 
      (if (zip/end? p) p (recur (zip/next p))))))

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

(defn mark-syntax [src-zip]
  (loop [loc (r/zipper (zip/root src-zip))]
    (let [n  (zip/node loc)
          k  (cond (keyword?* n)  :keyword
                   (core?* n)     :clj-core
                   (java?* n)     :java-cls
                   (symbol?* n)   :symbol
                   (string?* n)   :string
                   :else          :default)
          p  (try
               (zip/edit loc set-color k)
               (catch Throwable e loc))]
      (if (zip/end? p) p (recur (zip/next p))))))

(defn colorize [src-zip k color]
  (let [{c :color b :bold} (get colors color)]
    (loop [loc (r/zipper (zip/root src-zip))]
      (let [p? (get (meta (zip/node loc)) k)
            p  (try
                 (if p? (zip/edit loc vary-meta assoc :color c :bold b) loc) 
                 (catch Throwable e loc))]
        (if (zip/end? p) p (recur (zip/next p)))))))
