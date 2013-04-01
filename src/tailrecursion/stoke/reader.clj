(ns tailrecursion.stoke.reader
  (:require [clojure.java.io :as io]
            [clojure.core    :as core])
  (:import  [java.io PushbackReader
                     StringReader])
  (:refer-clojure :exclude [read read-string]))

(def delims       {\[ \] \{ \} \( \)})
(def macros       #{"^" "#" "#=" "#_" "#'" "'" "`" "~" "~@"})
(def char*        #(if (pos? %) (char %)))
(def eof?         #(not (pos? %)))
(def crlf?        #(= \newline (char* %)))
(def whitespace?  #(and (not (crlf? %))
                        (or (Character/isWhitespace %) (= (char* %) \,))))
(def delim?       #((set (mapcat identity delims)) (char* %)))
(def word?        #(not (or (eof? %) (whitespace? %) (crlf? %) (delim? %))))

(defn peek-ch [rdr]
  (let [ch (.read rdr)]
    (if (pos? ch) (.unread rdr ch))
    ch))

(defn gobble-whitespace [rdr]
  (loop [ch (.read rdr)]
    (if (whitespace? ch)
      (recur (.read rdr))
      (if (not (eof? ch)) (.unread rdr ch) ))))

(defn read-break [rdr]
  (gobble-whitespace rdr)
  (when (crlf? (peek-ch rdr))
    (.skip rdr 1)
    (loop [seen [(.read rdr)]]
      (let [c       (peek seen)
            unread  (fn [v] (mapv #(.unread rdr %) (reverse v)))]
        (cond (crlf? c)       :break
              (eof? c)        (do (unread (pop seen)) nil) 
              (whitespace? c) (recur (conj seen (.read rdr)))
              :else           (do (unread seen) (read-break rdr)))))))

(defn read-scalar [rdr]
  (gobble-whitespace rdr)
  (or (if (= \" (char* (peek-ch rdr))) (pr-str (clojure.core/read rdr)))
      (let [sb (StringBuffer.)]
        (loop [ch (.read rdr)]
          (if (not (word? ch)) 
            (do (.unread rdr ch) (if (not (empty? sb)) (str sb)))
            (do (.append sb (char* ch)) (recur (.read rdr))))))))
  
(declare read)

(defn read-sequence [rdr]
  (gobble-whitespace rdr)
  (when-let [open ((set (keys delims)) (char* (peek-ch rdr)))]
    (let [close (get delims open)]
      (.skip rdr 1) 
      (loop [items [] nxt (read rdr)]
        (if (not nxt)
          (let [ch (.read rdr)]
            (if (or (eof? ch) (not= close (char* ch)))
              (throw (Exception. "Unterminated sequence."))
              (with-meta items {:delims [open close]})))
          (recur (conj items nxt) (read rdr)))))))

(defn read [rdr]
  (or (read-break rdr)
      (read-sequence rdr)
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

  (pp "()") 
  ;=>
  [^{:delims [\( \)]} []]

  (pp "'()")
  ;=>
  [^{:prefix "'", :delims [\( \)]} []]

  (pp "#{1 2 3}")
  ;=>
  [^{:prefix "#", :delims [\{ \}]} ["1" "2" "3"]]

  (pp "(defn ^{:foo true} bar [x y] #(doit x y #\"and\" %))")
  ;=>
  [^{:delims [\( \)]} ["defn" ^{:prefix "^", :delims [\{ \}]} [":foo" "true"] "bar" ^{:delims [\[ \]]} ["x" "y"] ^{:prefix "#", :delims [\( \)]} ["doit" "x" "y" "#\"and\"" "%"]]]

  )

