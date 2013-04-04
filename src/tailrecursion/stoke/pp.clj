(ns tailrecursion.stoke.pp
  (:require
    [bbloom.fipp.printer        :as p :refer [defprinter]]
    [tailrecursion.stoke.reader :as r]))

(declare pretty)

(defn node
  ([tag]
   (node nil))
  ([tag indent]
   (node tag indent []))
  ([tag indent children] 
   (fn [& args]
     (let [render #(if (fn? %) (% ::render) %)]
       (if (= ::render (first args))
         (if (vector? children)
           (if indent
             `[~tag ~indent ~@(map render children)]
             `[~tag ~@(map render children)])
           tag) 
         (if (vector? children)
           (node tag indent (into children args))
           (throw (Exception. "Can't append children to leaf node."))))))))

(defn leaf [tag] (node tag nil nil))

(defn branch
  ([tag] (branch tag nil))
  ([tag indent] (node tag indent [])))

(def text   (branch :text))
(def pass   (branch :pass))
(def span   (branch :span))
(def line   (leaf   :line))
(def break  (leaf   :break))
(def group  (branch :group))
(def nest   (branch :nest 2))
(def align  (branch :align))

(def delims   #(:delims (meta %)))
(def prefix   #(:prefix (meta %)))

(defn cat
  ([x]
   (cat :line x))
  ([br x]
   (interpose br x)))

(defn type* [x]
  (let [p (str (prefix x)) 
        d (str (first (delims x)))]
    (cond
      (set? x)      :str
      (keyword? x)  :key
      (vector? x)   (case d
                      "{" (if (= "#" p) :set :map)
                      "(" :seq
                      "[" :vec
                      :spliced)
      :else         :sym)))

(defn by-pairs [x]
  (let [add #(conj %1 `[:group ~@(cat (map pretty %2))])]
    `[:align ~@(->> (reduce add [] (partition 2 x)) cat)]))

(defn pp-coll
  [x & body]
  (let [p (str (prefix x))
        d (delims x)
        l (str (first d))
        r (str (second d))]
    `[:group ~(str p l) ~@body ~r]))

(defn pp-seq-leading
  ([n x]
   (pp-seq-leading n x :line))
  ([n x br] 
   (if (< n (count x))
     (let [head `[:group ~@(cat (map pretty (take n x)))]
           body (cat (map pretty (drop n x)))]
       (pp-coll x `[:nest 2 ~head ~br ~@body])))))

(defmulti pp-seq #(str (first %)))

(defmethod pp-seq :default [x] (pp-seq-leading 1 x))
(defmethod pp-seq "defn" [x] (pp-seq-leading 2 x))
(defmethod pp-seq "defmethod" [x] (pp-seq-leading 3 x))

(defmethod pp-seq "let" [x]
  (if (and (< 2 (count x)) (= :vec (type* (second x)))) 
    (let [op    (pretty (first x))
          bind  ((get-method pretty :vec-pairs) (second x))
          head  `[:group ~op " " ~bind]
          body  (cat (map pretty (drop 2 x)))]
      (pp-coll x `[:nest 2 ~head :line ~@body]))))

(defmethod pp-seq "cond" [x]
  (if (< 1 (count x))
    (let [op    (pretty (first x))
          cases `[:nest 2 ~@(drop 1 (by-pairs (subvec x 1)))]]
      (pp-coll x op :line cases))))

(defmethod pp-seq "case"
  [x]
  (if (< 2 (count x))
    (let [op    (pretty (first x))
          expr  (pretty (second x))
          cases `[:group [:nest 2 ~@(drop 1 (by-pairs (subvec x 2)))]]]
      (pp-coll x [:group op :line expr] :line cases))))

(defmethod pp-seq "defproject"
  [x]
  (if (< 3 (count x))
    (let [head  `[:group ~@(cat (map pretty (take 3 x)))] 
          props `[:group [:nest 2 ~@(drop 1 (by-pairs (subvec x 3)))]]]
      (pp-coll x head :line props))))

(defmulti pretty type*)

(defmethod pretty :spliced [x]
  `[:nest 0 ~@(mapcat #(if (= :break %) [%] [% :line]) (map pretty x))])

(defmethod pretty :sym [x]
  (let [p (prefix x)
        d (delims x)]
    [:text (apply str p (first d) x (second d))]))

(defmethod pretty :str [x]
  (let [p (prefix x)
        d (delims x)
        s (let [y (first x)]
            (if (= "#" (str p))
              (subs (pr-str (re-pattern y)) 1)
              (pr-str y)))]
    [:text (str p (first d)) s (str (second d))]))

(defmethod pretty :set [x]
  (pp-coll x `[:align ~@(cat (map pretty x))]))

(defmethod pretty :key [x] x)
(defmethod pretty :seq [x] (pp-seq x))
(defmethod pretty :map [x] (pp-coll x (by-pairs x)))
(defmethod pretty :vec [x] ((get-method pretty :set) x))
(defmethod pretty :vec-pairs [x] ((get-method pretty :map) x))

(defprinter pprint pretty {:width 80})

(defmethod pp-seq "def" [x] ((get-method pp-seq "defn") x))
(defmethod pp-seq "fn" [x] ((get-method pp-seq "defn") x))
(defmethod pp-seq "ns" [x] ((get-method pp-seq "defn") x))
(defmethod pp-seq "if" [x] ((get-method pp-seq "defn") x))
(defmethod pp-seq "if-not" [x] ((get-method pp-seq "defn") x))
(defmethod pp-seq "when" [x] ((get-method pp-seq "defn") x))
(defmethod pp-seq "if-let" [x] ((get-method pp-seq "let") x))
(defmethod pp-seq "when-let" [x] ((get-method pp-seq "let") x))
(defmethod pp-seq "loop" [x] ((get-method pp-seq "let") x))
(defmethod pp-seq "binding" [x] ((get-method pp-seq "let") x))

