(ns tailrecursion.stoke.pp
  (:require
    [bbloom.fipp.printer :as p :refer [defprinter]]
    ))

(declare pretty pp-bindings-vec)

(def lines (partial interpose :line))

(defn pp-by-pairs [x]
  (let [pairs (map (fn [[k v]]
                     [:group (pretty k) :line (pretty v)])
                   (partition 2 (:val x)))]
    (into [:align] (interpose :line pairs))))

(defn pp-bindings-vec [x]
  [:group "[" (pp-by-pairs x) "]"])

(defmulti pp-list #(:val (first (:val %))))

(defmethod pp-list "condp" [x]
  (let [[op pred expr & body] (:val x)]
    [:group
     "(" [:group (pretty op) :line (pretty pred) :line (pretty expr)] :line
     ((into [:nest 2] (interpose :line (map pretty body))))
     ")"]))

(defmethod pp-list "if" [x]
  (let [[op pred & body] (:val x)]
    [:group
     "(" [:group (pretty op) :line (pretty pred)] :line
     (into [:nest 2] (interpose :line (map pretty body)))
     ")"]))

(defmethod pp-list "let" [x]
  (let [[op args & body] (:val x)]
    [:group
     "(" (pretty op) " " (-> [:nest 2]
                           (into (pp-bindings-vec args))
                           (conj :line)
                           (into (interpose :line (map pretty body))))
     ")"]))

(defmethod pp-list "def" [x]
  (let [[op name & body] (:val x)]
    [:group
     "(" [:group (pretty op) :line (pretty name)] :line
     (into [:nest 2] (interpose :line (map pretty body)))
     ")"]))

(defmethod pp-list "fn" [x]
  (let [[op name arglist & body] (:val x)
        has-name?   (= :scalar (:type name))
        body        (if has-name? body (cons arglist body))
        arglist     (if has-name? arglist name)
        name        (if has-name? name {:type :scalar :val ""})
        pad         (if has-name? :line "")]
    [:group
     "(" [:group (pretty op) :line (pretty name) pad (pretty arglist)] :line
     (into [:nest 2] (interpose :line (map pretty body))) ")"]))

(defmethod pp-list :default [x]
  [:group "(" [:nest 2 (interpose :line (map pretty (:val x)))] ")"])

(defmethod pp-list "if-not"       [x] ((get-method pp-list "if") x))
(defmethod pp-list "when"         [x] ((get-method pp-list "if") x))
(defmethod pp-list "when-not"     [x] ((get-method pp-list "if") x))
(defmethod pp-list "->"           [x] ((get-method pp-list "if") x))
(defmethod pp-list "->>"          [x] ((get-method pp-list "if") x))
(defmethod pp-list "case"         [x] ((get-method pp-list "if") x))

(defmethod pp-list "binding"      [x] ((get-method pp-list "let") x))
(defmethod pp-list "with-redefs"  [x] ((get-method pp-list "let") x))

(defn pp-vector [x]
  [:group "[" (into [:align] (interpose :line (map pretty (:val x)))) "]"])

(defn pp-map [x]
  [:group "{" (pp-by-pairs x) "}"])

(defn pp-set [x]
  [:group "#{" [:align (interpose :line (map pretty (:val x)))] "}"])

(defmulti pretty :type)

(defmethod pretty :default  [x] [:group (pr-str x)])
(defmethod pretty :scalar   [x] (:val x))
(defmethod pretty :list     [x] (pp-list x))
(defmethod pretty :vector   [x] (pp-vector x))
(defmethod pretty :map      [x] (pp-map x))

(defprinter pprint pretty {:width 80})

(comment
  
  (pprint {:type :list
           :val [{:type :scalar
                  :val "defn"}
                 {:type :scalar
                  :val "my-thing"}
                 {:type :vector
                  :val [{:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "y"}
                        {:type :scalar
                         :val "z"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "do"}
                        {:type :scalar
                         :val "some"}
                        {:type :scalar
                         :val "things"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 ]})

  (pprint {:type :list
           :val [
                 {:type :scalar
                  :val "fn"}
                 {:type :vector
                  :val [{:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "y"}
                        {:type :scalar
                         :val "z"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 {:type :list
                  :val [{:type :scalar
                         :val "doit"}
                        {:type :scalar
                         :val "x"}]}
                 ]})
                 {:type :vector
                  :val [
                        {:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "\"foo\""}
                        {:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "\"foo\""}
                        {:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "\"foo\""}
                        {:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "\"foo\""}
                        {:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "\"foo\""}
                        {:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "\"foo\""}
                        {:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "\"foo\""}
                        {:type :scalar
                         :val "x"}
                        {:type :scalar
                         :val "\"foo\""}]}

  (def x {:type :list
          :val [{:type :scalar
                 :val "defn"}
                {:type :scalar
                 :val "factorial"}
                {:type :vector
                 :val [{:type :scalar
                        :val "n"}]}
                {:type :list
                 :val [{:type :scalar
                        :val "reduce"}
                       {:type :scalar
                        :val "*"}
                       {:type :vector
                        :val [{:type :scalar
                               :val "n"}]}]}
                {:type :list
                 :val [{:type :scalar
                        :val "reduce"}
                       {:type :scalar
                        :val "*"}
                       {:type :vector
                        :val [{:type :scalar
                               :val "n"}]}]}
                {:type :list
                 :val [{:type :scalar
                        :val "reduce"}
                       {:type :scalar
                        :val "*"}
                       {:type :vector
                        :val [{:type :scalar
                               :val "n"}]}]}
                {:type :list
                 :val [{:type :scalar
                        :val "reduce"}
                       {:type :scalar
                        :val "*"}
                       {:type :vector
                        :val [{:type :scalar
                               :val "n"}]}]}
                {:type :list
                 :val [{:type :scalar
                        :val "reduce"}
                       {:type :scalar
                        :val "*"}
                       {:type :vector
                        :val [{:type :scalar
                               :val "n"}]}]}
                {:type :list
                 :val [{:type :scalar
                        :val "reduce"}
                       {:type :scalar
                        :val "*"}
                       {:type :vector
                        :val [{:type :scalar
                               :val "n"}]}]}
                {:type :list
                 :val [{:type :scalar
                        :val "reduce"}
                       {:type :scalar
                        :val "*"}
                       {:type :vector
                        :val [{:type :scalar
                               :val "n"}]}]}
                {:type :list
                 :val [{:type :scalar
                        :val "reduce"}
                       {:type :scalar
                        :val "*"}
                       {:type :vector
                        :val [{:type :scalar
                               :val "n"}]}]}
                ]}) 

  (pprint x)
  )

