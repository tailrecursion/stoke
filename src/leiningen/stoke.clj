(ns leiningen.stoke
  (:require
    [tailrecursion.stoke.edit     :as e]
    [tailrecursion.stoke.vikeys   :as v]
    [tailrecursion.stoke.screen   :as s]))

(defn stoke
  "I don't do a lot."
  [project file & args]
  (let [config      (:stoke project)
        ui          (or (:ui config) s/start)
        keybindings (or (:keybindings config) v/command)]
    (e/read-file file)
    (ui keybindings)))

(comment

  (code-str file)
  
  )
