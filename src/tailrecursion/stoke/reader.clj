(ns tailrecursion.stoke.reader
  (:require [clojure.java.io :as io]
            [clojure.core    :as core]
            [clojure.string  :as string])
  (:refer-clojure :exclude [read read-string]))

(def delims         {\[ \] \{ \} \( \)})
(def macros         #{"^" "#" "#=" "#_" "#'" "'" "`" "~" "~@" "@"})
(def macro?         #(and (symbol? %) (contains? macros (name %))))

(def re-scalar      #"^[^\s,\"\[\](){}]+")
(def re-linebreak   #"^\n\s*\n")
(def re-whitespace  #"^[\s,&&[^\n]]+")
(def re-macro       #"^(?:#'|#=|#_|\^|#|'|`|@|~@|~)")
(def re-re          #"^#\"")

(defn gobble-whitespace [src]
  (swap! src string/replace-first re-whitespace "")
  nil)

(defn by-pattern [p f src]
  (gobble-whitespace src)
  (when-let [s (re-find p @src)]
    (swap! src subs (count s))
    (f s)))

(defn read-break [src]
  (gobble-whitespace src)
  (if (= \newline (first @src))
    (if (not= @src (swap! src string/replace-first re-linebreak ""))
      :break
      (do (swap! src subs 1) nil))))

(defn read-re [src]
  (gobble-whitespace src)
  (try
    (when-let [p (and (re-find re-re @src) (core/read-string @src))]
      (swap! src subs (count (pr-str p))) 
      (with-meta #{(.pattern p)} {:prefix (symbol "#")}))
    (catch Throwable e
      (throw (Exception. (str "Unreadable regex: " @src) e)))))

(defn read-str [src]
  (gobble-whitespace src)
  (try 
    (when-let [s (and (= \" (first @src)) (core/read-string @src))] 
      (swap! src subs (count (pr-str s)))
      (into #{} [s]))
    (catch Throwable e
      (throw (Exception. (str "Unreadable string: " @src) e)))))

(defn read-macro [src]
  (by-pattern re-macro symbol src))

(defn read-char [src]
  (when-let [s (and (= \\ (first @src)) (apply str (take 2 @src)))] 
    (swap! src subs 2)
    (symbol s)))

(defn read-scalar [src]
  (by-pattern re-scalar symbol src))
  
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
  (or (read-re src)
      (read-macro src)
      (read-break src)
      (read-char src)
      (read-str src)
      (read-sequence src)
      (read-scalar src)))

(defn collapse-prefix [forms]
  (if (vector? forms)
    (let [collapse (fn [xs x]
                     (let [x (collapse-prefix x)]
                       (if (macro? (peek xs))
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

