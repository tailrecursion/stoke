(ns tailrecursion.stoke.mode.command
  (:require
    [clojure.zip              :as zip]
    [clojure.string           :as string]
    [tailrecursion.stoke.edit :as e]
    [tailrecursion.stoke.term :as t]))

(def mode   (atom nil))
(def prompt (atom ""))

(defn doit [x] (inc x))

(defn read-file [_]
  (reset! t/cmd "read-file>  \b")
  (reset! mode (fn [f] (t/read-file f) (t/set-mode! :normal))))

(defn write-file [_]
  (reset! t/cmd (if (e/write-file) "ok" "fail"))
  (.read System/in)
  (t/set-mode! :normal))

(defn evaluate [cmd-atom]
  (let [m @mode]
    (reset! mode nil)
    (if m (m (string/replace @t/cmd #"^.*\x08" "")) (char 27))))

(defn command-dispatch [c]
  (cond
    (= (char 27)  c)  (do (reset! mode nil) c) 
    (= (char 127) c)  (if (not= "\b" (str (last @t/cmd)))
                        (reset! t/cmd (apply str (butlast @t/cmd)))) 
    (= \newline   c)  (evaluate t/cmd)
    :else             (if @mode (reset! t/cmd (str @t/cmd c)) c))
  )
