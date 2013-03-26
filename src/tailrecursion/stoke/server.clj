(ns tailrecursion.stoke.server
  (:require
    [clojure.zip :as z]
    )
  )

(def src    (ref nil))
(def conch  (ref nil))

;; NAV
;; :down
;; :right
;;
;; OPS
;; :append-child
;;
