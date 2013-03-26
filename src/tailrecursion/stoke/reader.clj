(ns tailrecursion.stoke.reader
  (:require [clojure.java.io :as io]
            [clojure.core    :as core])
  (:import  [java.io PushbackReader
                     StringReader])
  (:refer-clojure :exclude [read read-string]))

(defn whitespace? [ch]
  (or (Character/isWhitespace ch) (= ch \,)))

(def delims
  {\[ \] \{ \} \( \)})

(defn read-string
  [rdr eof-value]
  {:type :scalar
   :val (pr-str (clojure.core/read rdr))})

(defn read-scalar
  [rdr eof-value]
  {:type :scalar
   :val (let [sb (StringBuffer.)]
          (loop [ch (.read rdr)]
            (if (neg? ch)
              (if (empty? sb)
                eof-value
                (str sb))
              (if (or (whitespace? (char ch))
                      ((set (mapcat identity delims)) (char ch)))
                (if (empty? sb)
                  (recur (.read rdr))
                  (do (.unread rdr ch)
                      (str sb)))
                (do (.append sb (char ch))
                    (recur (.read rdr)))))))})

(declare read)

(defn read-sequence
  [rdr type delim eof-value]
  {:type :sequence
   :val (loop [s []]
          (let [ch (.read rdr)]
            (if (= delim (char ch))
              s
              (do (.unread rdr ch)
                  (recur (conj s (read rdr eof-value)))))))})

(defn read
  [rdr eof-value]
  (let [ch (.read rdr)]
    (if (pos? ch)
      (do (.unread rdr ch)
          (if-let [delim (delims (char ch))]
            (read-sequence rdr delim eof-value)
            (read-scalar rdr eof-value)))
      eof-value)))

(defn read-file
  [file]
  (with-open [rdr (PushbackReader. (io/reader file))]
    (loop [forms []]
      (let [form (read rdr ::eof)]
        (if (= form ::eof)
          forms
          (recur (conj forms form)))))))

(comment

  (defn p [s]
    (PushbackReader. (StringReader. s)))
  
  (read-scalar (p "abc"))

  )

;; (defn factorial
;;   "Returns factorial of n"
;;   [n]
;;   (reduce * (range 1 (inc n))))

;; {:type :list
;;  :val [{:type :scalar
;;         :val "defn"}
;;        {:type :scalar
;;         :val "factorial"}
;;        {:type :vector
;;         :val [{:type :scalar
;;                :val "n"}]}
;;        {:type :list
;;         :val [{:type :scalar
;;                :val "reduce"}
;;               {:type :scalar
;;                :val "*"}
;;               {:type :vector
;;                :val [{:type :scalar
;;                       :val "n"}]}]}]}
