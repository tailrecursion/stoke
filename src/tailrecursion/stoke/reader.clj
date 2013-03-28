(ns tailrecursion.stoke.reader
  (:require [clojure.java.io :as io]
            [clojure.core    :as core])
  (:import  [java.io PushbackReader
                     StringReader])
  (:refer-clojure :exclude [read read-string]))

(def delims       {\[ \] \{ \} \( \)})
(def macros       #{"^" "#" "#=" "#_" "#'" "'" "`" "~" "~@"})
(def whitespace?  #(or (Character/isWhitespace %) (= % \,)))
(def eof?         #(not (pos? %)))
(def char*        #(if (pos? %) (char %)))

(defn peek-ch [rdr]
  (let [ch (.read rdr)]
    (if (pos? ch) (.unread rdr ch))
    ch))

(defn gobble-whitespace [rdr]
  (loop [ch (.read rdr)]
    (if (whitespace? ch)
      (recur (.read rdr))
      (do (if (not (eof? ch)) (.unread rdr ch)) 
        rdr))))

(defn read-scalar [rdr]
  (gobble-whitespace rdr)
  (or (if (= \" (char* (peek-ch rdr))) (pr-str (clojure.core/read rdr)))
      (let [sb (StringBuffer.)
            end? #(or (eof? %) (whitespace? %) ((set (mapcat identity delims)) (char* %)))]
        (loop [ch (.read rdr)]
          (if (end? ch)
            (do (.unread rdr ch) (if (not (empty? sb)) (str sb)))
            (do (.append sb (char* ch)) (recur (.read rdr))))))))
  
(declare read)

(defn read-sequence [rdr]
  (gobble-whitespace rdr)
  (when-let [open (get (set (keys delims)) (char* (peek-ch rdr)))]
    (let [close (get delims open)
          end? #(or (eof? %) (= close (char* %)))]
      (.skip rdr 1) 
      (loop [items []]
        (gobble-whitespace rdr)
        (let [ch (.read rdr)]
          (if (end? ch)
            (with-meta items {:delims [open close]})
            (do (.unread rdr ch) (recur (conj items (read rdr))))))))))

(defn read [rdr]
  (or (read-sequence rdr)
      (read-scalar rdr)))

(defn collapse-prefix [forms]
  (if (vector? forms)
    (let [collapse (fn [xs x]
                     (let [x (collapse-prefix x)]
                       (if (and (contains? macros (peek xs)) (vector? x))
                         (conj (pop xs) (vary-meta x merge {:prefix (peek xs)})) 
                         (conj xs x))))]
      (with-meta (reduce collapse [] forms) (meta forms)))
    forms))

(defn read-all [rdr]
  (collapse-prefix
    (loop [forms []]
      (if-let [form (read rdr)]
        (recur (conj forms form))
        forms))))

(defn read-file
  [file]
  (with-open [rdr (PushbackReader. (io/reader file))]
    (read-all rdr)))

(comment

  (defn p [s]
    (PushbackReader. (StringReader. s)))
  
  (defn pp [s]
    (binding [*print-meta* true]
      (prn (read-all (p s)))) )

  (pp "(defn ^{:foo true} bar [x y] #(doit x y \"and\" %))")
  ;=>
  [^{:delims [\( \)]} ["defn" ^{:prefix "^", :delims [\{ \}]} [":foo" "true"] "bar" ^{:delims [\[ \]]} ["x" "y"] ^{:prefix "#", :delims [\( \)]} ["doit" "x" "y" "\"and\"" "%"]]]

  )

