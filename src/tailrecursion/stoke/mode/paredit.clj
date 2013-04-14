(ns tailrecursion.stoke.mode.paredit
  (:require
    [clojure.zip                        :as zip]
    [tailrecursion.stoke.term.constants :refer [ESC DEL C-X BOX BLOCK]]
    [tailrecursion.stoke.edit           :as e]
    [tailrecursion.stoke.read           :as r]
    [tailrecursion.stoke.cmd            :as c]
    [tailrecursion.stoke.util           :as u]
    [tailrecursion.stoke.print          :as p]))

(declare placeholder)

(def ^:private
  word?
  #(and (not (Character/isISOControl %))
        (not (Character/isWhitespace %))))

(def ^:private
  trim
  #(apply str (drop-last %2 %1)))

(def ^:private
  read-thing
  #(if (string? %) (r/read-string %) %))

(def ^:private
  read-op
  #(fn [s] (e/edit % (read-thing s))))

(def ^:private
  enter-mode
  #((read-op %) placeholder))

(def ^:private
  rpl-point
  (read-op c/replace-point))

(def ^:private
  ins-left
  (read-op c/insert-left))

(def ^:private
  ins-leftmost
  (read-op c/insert-leftmost))

(def ^:private
  ins-rightmost-child
  (read-op c/insert-rightmost-child))

(def ^:private
  ins-right
  (read-op c/insert-right))

(def ^:private
  ins-rightmost
  (read-op c/insert-rightmost))

(def ^:private
  rm-point
  #(e/edit c/delete-point))

(def ^:private
  edit-point
  #(->> (with-meta (read-thing %1) (meta %2))
     (e/edit c/replace-point)))

(def ^:private
  rm-left
  #(e/edit c/delete-left))

(def ^:private
  rm-last-child
  #(e/edit c/delete-rightmost-child))

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

(defn paredit-mode-break-before [_]
  (ins-left "\n\n")
  (ins-left placeholder))

(defn paredit-mode-break-after [_]
  (ins-right "\n\n")
  (ins-right placeholder))

(defn paredit-kill-prefix [_]
  (e/edit zip/edit vary-meta dissoc :prefix))

(defn- join-macro-left [z]
  (let [l (zip/left (zip/node z))
        s (if l (str (zip/node l)))]
    (when (contains? r/macros s)
      (rm-left)
      (e/edit zip/edit vary-meta assoc :prefix (symbol s)))))

(defn- join-macro-right [z]
  (let [n (zip/node (zip/node z))]
    (when (and (zip/right (zip/node z)) (contains? r/macros (str n)))
      (rm-point)
      (e/edit zip/edit vary-meta assoc :prefix (symbol (str n))))))

(defn- dispatch-quit [point point-zip type c placeholder?]
  (when (= ESC c)
    (if (or (= BOX (last (str point))) placeholder?) (rm-point))
    (join-macro-left @@e/point)
    (join-macro-right @@e/point)
    c))

(defn- dispatch-break [point point-zip type c placeholder?]
  (when (and placeholder? (= \newline c))
    (rpl-point "\n\n")
    (ins-right placeholder)))

(defn- dispatch-open [point point-zip type c placeholder?]
  (when (contains? (set (keys r/delims)) c)
    (join-macro-left
      ((if placeholder? rpl-point ins-right) (str c (r/delims c))))
    (ins-rightmost-child placeholder)))

(defn- dispatch-close [point point-zip type c placeholder?]
  (let [n (-> point-zip zip/up)
        p (if n (zip/node n))]
    (when (= c (get-in (meta p) [:delims 1]))
      (e/edit c/move-up)
      (if placeholder? (rm-last-child))
      (ins-right placeholder))))

(defn- dispatch-char [point point-zip type c placeholder?]
  (if (= :sym type)
    (let [charpoint?  (= (str point) (str \\ BOX))]
      (cond
        (and placeholder? (= \\ c)) (rpl-point (str \\ BOX))
        (and charpoint? (= DEL c))  (rpl-point placeholder)
        (and charpoint? (word? c))  (rpl-point (str \\ c))))))

(defn- dispatch-delete [point point-zip type c placeholder?]
  (if (and placeholder? (= DEL c)) true))

(defn- dispatch-scalar [point point-zip type c placeholder?]
  (let [ok? (re-find r/re-scalar (str c))
        s   (str point)]
    (cond
      (and ok? placeholder?)
      (join-macro-left (rpl-point (str c)))
      (and (not placeholder?) (= :sym type))
      (cond
        (= DEL c)       (if (< 1 (count s))
                          (edit-point (trim s 1) point)
                          (rpl-point placeholder))
        (= \space c)    (ins-right placeholder)
        ok?             (edit-point (str s c) point)))))

(defn- dispatch-macro [point point-zip type c placeholder?]
  (let [m? #(contains? r/macros (apply str %&))]
    (cond
      (and (m? c) placeholder?) (rpl-point (str c))
      (m? point c)              (rpl-point (str point c)))))

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
    (or (dispatch dispatch-delete)
        (dispatch dispatch-quit)
        (dispatch dispatch-char)
        (dispatch dispatch-macro)
        (dispatch dispatch-string)
        (dispatch dispatch-scalar)
        (dispatch dispatch-open)
        (dispatch dispatch-close)
        (dispatch dispatch-break)
        c)))
