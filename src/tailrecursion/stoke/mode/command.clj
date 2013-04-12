(ns tailrecursion.stoke.mode.command
  (:require
    [clojure.zip              :as zip]
    [tailrecursion.stoke.edit :as e]
    [tailrecursion.stoke.term :as t]
    )
  )

(defn doit [x] (inc x))

(defn evaluate [cmd-atom]
  (t/read-file @cmd-atom)
  (char 27))

(defn command-dispatch [c]
  (cond
    (= (char 27)  c)  c
    (= (char 127) c)  (reset! t/cmd (apply str (butlast @t/cmd)))
    (= \newline   c)  (evaluate t/cmd)
    :else             (reset! t/cmd (str @t/cmd c))
    
    )
  )
