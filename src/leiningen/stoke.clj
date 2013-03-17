(ns leiningen.stoke
  (:require
    [clojure.string     :as string]
    [clojure.pprint     :as pp]
    [lanterna.screen    :as s]
    [lanterna.terminal  :as t]))

(declare pp-form)

(def scr    (atom nil))
(def forms  (atom nil))
(def curx   (atom 0))
(def cury   (atom 0))

(def file "/home/micha/src/checkout/javelin/src/cljs/tailrecursion/javelin/core.cljs")

(defn pp-form [form]
  (binding [*print-meta*                true
            pp/*print-pprint-dispatch*  pp/code-dispatch]
    (with-out-str (pp/pprint form))))

(def ^:dynamic *pprinter* pp-form)

(defn code-str [f]
  (->> (str "(" (slurp f) ")")
    read-string
    (mapv *pprinter*)
    (string/join "\n")))

(defn form-str [src]
  (loop [n 0 ret ""]
    (if (< n (count src))
      (let [f (subs src 0 n)
            x (try (read-string f)
                (catch Throwable e ::nope))]
        (if (or (= ::nope x) (not= (string/trim ret) (string/trim f)))
          (recur (inc n) f)
          f))
      (throw (Exception. (str "unreadable form: " src))))))

(defn draw [& _]
  (s/clear @scr)
  (let [fg          (atom :white)
        endx        (atom @curx)
        endy        (atom @cury)
        [cols rows] (s/get-size @scr)]
    (loop [line 0 lines (string/split @forms #"\n")]
      (let [this-line   (first lines)
            rest-lines  (rest lines)]
        (if (and (< @curx (count this-line)) (= @cury line)) 
          (let [src         (subs (string/join "\n" lines) @curx)
                form        (form-str src)
                form-lines  (string/split form #"\n")
                startx      (if (= 1 (count form-lines)) (inc @curx) 0)]
            (reset! endx (+ startx (count (last form-lines))))
            (reset! endy (+ @cury (dec (count form-lines)))))) 
        (cond
          (and (< @curx (count this-line)) (= line @endy) (= line @cury)) 
          (do
            (s/put-string @scr 0 line (subs this-line 0 @curx))
            (s/put-string @scr @curx line (subs this-line @curx (- @endx @curx)) {:fg :yellow})
            (s/put-string @scr @endx line (format (format "%%%ds" (- cols @endx)) " ")))

          (and (< @curx (count this-line)) (= line @cury)) 
          (do
            (s/put-string @scr 0 line (subs this-line 0 @curx))
            (s/put-string @scr @curx line
                          (format (format "%%-%ds" (- cols @curx))
                                  (subs this-line @curx))
                          {:fg :yellow}))

          (and (< @curx (count this-line)) (< line @endy) (> line @cury))
          (s/put-string @scr 0 line (format (format "%%-%ds" cols) this-line) {:fg :yellow})
            

          (and (< @curx (count this-line)) (= line @endy)) 
          (do
            (s/put-string @scr 0 line (subs this-line 0 @endx) {:fg :yellow})
            (if (< @endx (count this-line))
              (s/put-string @scr @endx line (subs this-line @endx))))

          :else
          (s/put-string @scr 0 line (format (format "%%-%ds" cols) this-line))) 
        (if (and (seq rest-lines) (< line (dec rows))) 
          (recur (inc line) (rest lines)))))) 
  (s/redraw @scr)
  ::ok)

(defn cursor [dir]
  (let [[cols rows] (s/get-size @scr)]
    (cond
      (and (= :left dir) (< 0 @curx))
      (swap! curx dec)

      (and (= :right dir) (< @curx (dec cols)))
      (swap! curx inc)

      (and (= :down dir) (< @cury (dec rows)))
      (swap! cury inc)

      (and (= :up dir) (< 0 @cury))
      (swap! cury dec))
    (s/move-cursor @scr @curx @cury)
    (draw)))

(defn stoke
  "I don't do a lot."
  [project & args]
  (reset! scr   (s/get-screen :unix {:resize-listener draw})) 
  (reset! forms (code-str file))
  (s/in-screen
    @scr
    (draw)
    (loop []
      (if (case (s/get-key-blocking @scr)
            \q nil
            \h (cursor :left)
            \l (cursor :right)
            \j (cursor :down)
            \k (cursor :up)
            (draw))
        (recur)))))

(comment

  (code-str file)
  
  )
