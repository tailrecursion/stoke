(ns tailrecursion.stoke.reader
  (:require [clojure.java.io :as io]
            [clojure.core    :as core]
            [clojure.string  :as string])
  (:refer-clojure :exclude [read read-string]))

(def delims       {\[ \] \{ \} \( \)})
(def macros       #{"^" "#" "#=" "#_" "#'" "'" "`" "~" "~@"})
(def macro?       #(and (symbol? %) (contains? macros (name %))))

(defn gobble-whitespace [src]
  (swap! src string/replace-first #"^[\s,&&[^\n]]+" "")
  nil)

(defn read-break [src]
  (gobble-whitespace src)
  (if (= \newline (first @src))
    (if (not= @src (swap! src string/replace-first #"^\n\s*\n" ""))
      :break
      (do (swap! src subs 1) nil))))

(defn read-str [src]
  (gobble-whitespace src)
  (when-let [s (and (= \" (first @src)) (core/read-string @src))] 
    (swap! src subs (+ 2 (count s)))
    (into #{} [s])))

(defn read-scalar [src]
  (gobble-whitespace src)
  (when-let [s (re-find #"^[^\s,\"\[\](){}]+" @src)]
    (swap! src subs (count s))
    (symbol s)))
  
(declare read)

(defn read-sequence [src]
  (gobble-whitespace src)
  (when-let [open ((set (keys delims)) (first @src))]
    (let [close (get delims open)]
      (swap! src subs 1)
      (loop [items [] nxt (read src)]
        (if (not nxt)
          (let [ch (first @src)]
            (if (or (not ch) (not= close ch))
              (throw (Exception. (str "Unterminated sequence: " @src))))
            (swap! src subs 1)
            (with-meta items {:delims [open close]}))
          (recur (conj items nxt) (read src)))))))

(defn read [src]
  (or (read-break src)
      (read-str src)
      (read-sequence src)
      (read-scalar src)))

(defn collapse-prefix [forms]
  (if (vector? forms)
    (let [collapse (fn [xs x]
                     (let [x (collapse-prefix x)]
                       (if (and (macro? (peek xs)) (coll? x))
                         (conj (pop xs) (vary-meta x merge {:prefix (peek xs)})) 
                         (conj xs x))))]
      (with-meta (reduce collapse [] forms) (meta forms)))
    forms))

(defn read-all [src]
  (collapse-prefix
    (loop [forms []]
      (if-let [form (read src)]
        (recur (conj forms form))
        forms))))

(defn read-file
  [file]
  (read-all (atom (slurp file))))

(defn read-string
  [s]
  (read (atom s)))

(comment

  (read-string "{:foo  \n  \n bar}")
  (read-string "\n \"bar\"")
  (read-string "(1 2\n 3)")

  (binding [*print-meta* true]
    (spit "out.stk" (pr-str (read-file "project.clj")))) 

  (with-open [r (java.io.PushbackReader. (io/reader "out.stk"))]
    (core/read r))
  

  )

