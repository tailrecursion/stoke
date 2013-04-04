(ns tailrecursion.stoke.pp
  (:require
    [bbloom.fipp.printer        :as p :refer [defprinter]]
    [tailrecursion.stoke.reader :as r]))

(declare pretty)

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
  (let [add-group #(conj %1 (into [:group] (cat (map pretty %2))))]
  (->>
    (reduce add-group [] (partition 2 x))
    (cat :break)
    (into [:align]))))

(defn pp-coll
  [x & body]
  (let [p (str (prefix x))
        d (delims x)
        l (str (first d))
        r (str (second d))]
    (into [:group (str p l)] (conj (vec body) r))))

(defn pp-seq-leading
  [n x]
  (let [head (into [:group] (cat (map pretty (take n x))))
        body (cat (map pretty (drop n x)))
        nest (into (if (< n (count x)) [head :line] [head]) body)]
    (pp-coll x (into [:nest 2] nest))))

(defmulti pp-seq #(str (first %)))

(defmethod pp-seq :default [x] (pp-seq-leading 1 x))

(defmethod pp-seq "defn" [x] (pp-seq-leading 2 x))

(defmethod pp-seq "defmethod" [x] (pp-seq-leading 3 x))

(defmethod pp-seq "let" [x]
  (let [op    (pretty (first x))
        bind  (let [b (second x)] 
                ((if (vector? b) (get-method pretty :vec-pairs) pretty) b))
        head  (into [:group] (cat " " (keep identity [op bind])))
        body  (cat (map pretty (drop 2 x)))
        nest  (into (if (< 2 (count x)) [head :break] [head]) body)]
    (pp-coll x (into [:nest 2] nest))))

(defmethod pp-seq "cond" [x]
  (let [op    (pretty (first x))
        cases (into [:nest 2] (drop 1 (by-pairs (subvec x 1))))]
    (pp-coll x op :line cases)))

(defmulti pretty type*)

(defmethod pretty :spliced [x]
  (into [:nest 0] (mapcat #(if (= :break %) [%] [% :line]) (map pretty x))))

(defmethod pretty :sym [x]
  (let [p (prefix x)
        d (delims x)]
    [:text (apply str p (first d) x (second d))]))

(defmethod pretty :str [x]
  ((get-method pretty :sym) (symbol (pr-str (first x)))))

(defmethod pretty :key [x] x)

(defmethod pretty :seq [x] (pp-seq x))

(defmethod pretty :set [x]
  (pp-coll x (into [:align] (cat (map pretty x)))))

(defmethod pretty :map [x]
  (pp-coll x (by-pairs x)))

(defmethod pretty :vec [x] ((get-method pretty :set) x))

(defmethod pretty :vec-pairs [x] ((get-method pretty :map) x))

(defprinter pprint pretty {:width 80})

(defmethod pp-seq "case" [x] ((get-method pp-seq "defn") x))

(defmethod pp-seq "def" [x] ((get-method pp-seq "defn") x))

(defmethod pp-seq "ns" [x] ((get-method pp-seq "defn") x))

(defmethod pp-seq "if" [x] ((get-method pp-seq "defn") x))

(defmethod pp-seq "loop" [x] ((get-method pp-seq "let") x))

