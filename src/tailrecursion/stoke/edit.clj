(ns tailrecursion.stoke.edit
  (:require
    [clojure.zip                :as zip]
    [tailrecursion.stoke.reader :as r]
    [tailrecursion.stoke.pp     :as pp]))

(defn meta-zip [root]
  (let [branch?   #(instance? clojure.lang.IMeta %)
        children  #(get (meta %) ::children)
        make-node #(vary-meta %1 assoc ::children %2)]
    (zip/zipper branch? children make-node root)))

(let [ok?   #(not (or (nil? %) (nil? (zip/node %))))
      init  (meta-zip (zip/vector-zip []))]
  (def point (atom init :validator ok?)))

(defn pprint []
  (print "\033[2J")
  (let [post #(if (identical? %1 (zip/node (zip/node @point)))
                [:span [:pass "\033[38;5;154m"] %2 [:pass "\033[0m"]]
                %2)]
    (binding [pp/post-process post]
      (pp/pprint (zip/root (zip/node @point))))))

(defn add-history [x]
  (swap! point #(zip/rightmost (zip/down (zip/append-child % x)))))

(defn read-file [f]
  (add-history (zip/down (r/zipper (r/read-file f)))))

(defn edit [f & args]
  (try
    (let [prv (zip/root (zip/node @point))
          p   (apply f (zip/node @point) args)
          nxt (zip/root p)]
      (if (= prv nxt)
        (swap! point zip/replace p)
        (add-history p)))
    (catch Throwable e))
  (pprint))

(defn undo [f & args]
  (try
    (apply swap! point f args)
    (catch Throwable e)) 
  (pprint))

;;; In order for this to work without having to press enter, you must
;;; set stty before running the JVM:
;; stty -icanon min 1
(defn -main [file]
  (read-file file)
  (loop []
    (pprint)
    (let [c (char (.read System/in))]
      (when-not (= \q c)
        (case c
          \p (pprint)
          \k (edit zip/up)
          \j (edit zip/down)
          \h (edit zip/left)
          \l (edit zip/right)
          \d (edit zip/remove)
          \c (edit zip/replace (r/read-string (pr-str (read))))
          \u (undo zip/up)
          \r (undo zip/down)
          \t (undo zip/left)
          \y (undo zip/right)
          nil)
        (recur)))))
