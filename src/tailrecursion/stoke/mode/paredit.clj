(ns tailrecursion.stoke.mode.paredit
  (:require
    [clojure.zip                      :as zip]
    [tailrecursion.stoke.edit         :as e]
    [tailrecursion.stoke.read         :as r]
    [tailrecursion.stoke.cmd          :as c]
    [tailrecursion.stoke.print        :as p]))

(declare placeholder)

(let [ESC                 (char 27)
      DEL                 (char 127)
      BOX                 \u25A1
      BLOCK               \u2588
      word?               #(not (Character/isWhitespace %)) 
      trim                #(apply str (drop-last %2 %1))
      read-thing          #(if (string? %) (r/read-string %) %)
      read-op             #(fn [s] (e/edit % (read-thing s)))
      enter-mode          #((read-op %) placeholder)
      rpl-point           (read-op c/replace-point)
      ins-left            (read-op c/insert-left)
      ins-leftmost        (read-op c/insert-leftmost)
      ins-rightmost-child (read-op c/insert-rightmost-child)
      ins-right           (read-op c/insert-right)
      ins-rightmost       (read-op c/insert-rightmost)
      rm-point            #(e/edit c/delete-point)
      ins-sequential      #(let [op (if (= placeholder (str (e/get-point)))
                                      c/replace-point
                                      c/insert-right)] 
                             ((read-op op) %) 
                             (ins-rightmost-child placeholder))
      cleanup             #(let [p (e/get-point)
                                 s (last (str p))]
                             (if (or (= BOX s) (::placeholder (meta p)))
                               (rm-point)))]

  (def placeholder
    (-> (symbol (str BLOCK)) (with-meta {::placeholder true})))

  (defn paredit-mode-left [_]
    (ins-left placeholder))

  (defn paredit-mode-leftmost [_]
    (ins-left placeholder))

  (defn paredit-mode-right [_]
    (ins-right placeholder))

  (defn paredit-mode-rightmost [_]
    (ins-rightmost placeholder))

  (defn paredit-mode-rightmost-child [_]
    (ins-rightmost-child placeholder))

  (defn paredit-mode-edit [_] true)

  (defn paredit-mode-replace [_]
    (rpl-point placeholder))

  (defn- dispatch-quit [point point-zip type c placeholder?]
    (when (= ESC c)
      (if (or (= BOX (last (str point))) placeholder?) (rm-point))
      c))

  (defn- dispatch-break [point point-zip type c placeholder?]
    (when (and placeholder? (= \newline c)) 
      (rpl-point "\n\n")
      (ins-right placeholder)))

  (defn- dispatch-open [point point-zip type c placeholder?]
    (when (contains? (set (keys r/delims)) c)
      ((if placeholder? rpl-point ins-right) (str c (r/delims c)))
      (ins-rightmost-child placeholder)))

  (defn- dispatch-close [point point-zip type c placeholder?]
    (let [p (-> point-zip zip/up zip/node)]
      (when (= c (get-in (meta p) [:delims 1]))
        (if placeholder? (rm-point) (e/edit c/move-up)) 
        (ins-right placeholder))))

  (defn- dispatch-char [point point-zip type c placeholder?]
    (let [charpoint?  (= (str point) (str \\ BOX))]
      (cond
        (and placeholder? (= \\ c)) (rpl-point (str \\ BOX))
        (and charpoint? (= DEL c))  (rpl-point placeholder)
        (and charpoint? (word? c))  (rpl-point (str \\ c)))))

  (defn- dispatch-scalar [point point-zip type c placeholder?]
    (let [ok? (re-find r/re-scalar (str c))
          s   (str point)]
      (cond
        (and ok? placeholder?)
        (rpl-point (str c))
        (= :sym type)
        (cond
          (= DEL c)       (if (< 0 (count s))
                            (rpl-point (trim s 1))
                            (rpl-point placeholder))
          (= \space c)    (ins-right placeholder)
          ok?             (rpl-point (str s c))))))

  (defn- dispatch-string [point point-zip type c placeholder?]
    (cond
      (and placeholder? (= \" c))
      (rpl-point "\"\"")
      (= :str type)
      (let [s     (subs (trim (first point) 1) 1)
            esc?  (and (= BOX (last s)) (word? c))
            bad?  (and (= BOX (last s)) (not esc?))
            bak   #(cond
                     (= \\ (last (trim s 1)))  (rpl-point (str \" (trim s 2) \"))
                     (< 0 (count s))           (rpl-point (str \" (trim s 1) \"))
                     :else                     (rpl-point placeholder))]
        (cond
          (= DEL c)   (bak)
          esc?        (rpl-point (str \" (trim s 1) c \"))
          (= \\ c)    (rpl-point (str \" s c BOX \"))
          (= \" c)    (ins-right placeholder)
          (not bad?)  (rpl-point (str \" s c \"))))))

  (defn paredit-dispatch [c]
    (let [point         (e/get-point)
          point-zip     (zip/node @@e/point) 
          type          (p/type* point)
          placeholder?  (::placeholder (meta point))
          dispatch      #(% point point-zip type c placeholder?)]
      (or (dispatch dispatch-quit)
          (dispatch dispatch-char)
          (dispatch dispatch-string)
          (dispatch dispatch-scalar)
          (dispatch dispatch-open)
          (dispatch dispatch-close)
          (dispatch dispatch-break)
          c)))

  )
