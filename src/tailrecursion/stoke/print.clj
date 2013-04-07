(ns tailrecursion.stoke.print
  (:require
    [bbloom.fipp.printer      :as p :refer [defprinter]]
    [tailrecursion.stoke.read :as r]))

(declare pretty)

(def ^:dynamic post-process (fn [x y] y))

(def delims   #(:delims (meta %)))
(def prefix   #(:prefix (meta %)))

(defn color [x s]
  (let [{c :color b :bold} (meta x)]
    [[:pass (format "\033[%dm\033[38;5;%dm" (or b 0) (or c 7))]
     [:text s] 
     [:pass "\033[0m"]]))

(defn cat
  ([x]
   (cat :line x))
  ([br x]
   (interpose br x)))

(defn type* [x]
  (let [p (str (prefix x)) 
        d (str (first (delims x)))]
    (cond
      (set? x)    :str
      (map? x)    :key
      (vector? x) (case d
                    "{" (if (= "#" p) :set :map)
                    "(" :seq
                    "[" :vec
                    :spliced)
      :else       :sym)))

(defn by-pairs [x]
  (let [add #(conj %1 `[:group ~@(cat (map pretty (keep identity %2)))])]
    `[:align ~@(cat (reduce add [] (partition 2 2 [nil] x)))]))

(defn pp-coll
  [x & body]
  (let [p (str (prefix x))
        d (delims x)
        l (str (first d))
        r (str (second d))]
    `[:group ~@(color x (str p l)) ~@body ~@(color x r)]))

(defn pp-seq-leading
  ([n x]
   (pp-seq-leading n x :line))
  ([n x br] 
   (if (< n (count x))
     (let [head `[:group ~@(cat (map pretty (take n x)))]
           body (cat (map pretty (drop n x)))]
       (pp-coll x `[:nest 2 ~head ~br ~@body]))
     (pp-coll x `[:group "" ~@(cat (map pretty x))]))))

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

(defn post [src node] (if node (post-process src node) node))

(defmulti pretty type*)

(defmethod pretty :spliced [x]
  (let [exprs (mapcat #(if (map? %) [(:key %)] [% :line]) (map pretty x))]
    (post x `[:nest 0 ~@exprs])))

(defmethod pretty :sym [x]
  (let [p (prefix x)
        d (delims x)]
    (post x `[:span ~@(color x (str p (first d) x (second d)))])))

(defmethod pretty :str [x]
  (let [p (prefix x)
        d (delims x)
        s (first x)]
    (post x `[:span ~@(color x (str p (first d) s (second d)))])))

(defmethod pretty :set [x]
  (post x (pp-coll x `[:align ~@(cat (map pretty x))])))

(defmethod pretty :key [x] (post x (:key x)))
(defmethod pretty :seq [x] (post x (pp-seq x)))
(defmethod pretty :map [x] (post x (pp-coll x (by-pairs x))))
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

