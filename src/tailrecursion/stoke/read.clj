(ns tailrecursion.stoke.read
  (:require
    [clojure.java.io :as io]
    [clojure.core    :as core]
    [clojure.string  :as string]
    [clojure.zip     :as zip])
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

(defn raw-read-str [s]
  (if (= \" (first s))
    (loop [out "\"" in (subs s 1)]
      (let [[p q & r] in]
        (cond
          (= \" p)          (str out p)
          (= [\\ \"] [p q]) (recur (str out p q) (subs in 2))
          p                 (recur (str out p) (subs in 1))
          :else             (throw (Exception. "Unreadable string: " in)))))))

(defn by-pattern [p f src]
  (gobble-whitespace src)
  (when-let [s (re-find p @src)]
    (swap! src subs (count s))
    (f s)))

(defn read-break [src]
  (gobble-whitespace src)
  (if (= \newline (first @src))
    (if (not= @src (swap! src string/replace-first re-linebreak ""))
      {:key :break :id (str (gensym))} 
      (do (swap! src subs 1) nil))))

(defn read-re [src]
  (gobble-whitespace src)
  (when-let [p (and (re-find re-re @src) (raw-read-str (subs @src 1)))]
    (swap! src subs (inc (count p)))
    (with-meta (into #{} [p]) {:prefix (symbol "#")})))

(defn read-str [src]
  (gobble-whitespace src)
  (when-let [s (and (= \" (first @src)) (raw-read-str @src))] 
    (swap! src subs (count s))
    (into #{} [s])))

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

(defn zipper [forms]
  (zip/zipper vector? seq #(with-meta (vec %2) (meta %1)) forms))
