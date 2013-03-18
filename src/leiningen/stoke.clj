(ns leiningen.stoke
  (:require
    [tailrecursion.stoke.edit     :as e]
    [tailrecursion.stoke.vikeys   :as v]
    [tailrecursion.stoke.screen   :as s]))

(defn stoke
  "I don't do a lot."
  [project file & args]
  (let [{u :ui k :keybindings :or {u s/start k v/command}} (:stoke project)]
    (e/read-file file)
    (u k)))

(comment

  (code-str file)
  
  )
