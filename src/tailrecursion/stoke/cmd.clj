(ns tailrecursion.stoke.cmd
  (:require
    [clojure.zip              :as zip]
    [tailrecursion.stoke.read :as r]
    [tailrecursion.stoke.util :as u]))

(defn move-left [z]           (u/repeat-while z map? zip/left))
(def  move-leftmost           zip/leftmost)
(defn move-right [z]          (u/repeat-while z map? zip/right))
(def  move-rightmost          zip/rightmost)
(def  move-down               zip/down)
(def  move-up                 zip/up)
(def  move-next               zip/next)
(def  move-prev               zip/prev)

(def  delete-point            u/remove-point)
(def  delete-left             u/remove-left)
(def  delete-lefts            u/remove-lefts)
(def  delete-right            u/remove-right)
(def  delete-rights           u/remove-rights)
(def  delete-parent           u/remove-parent)
(def  delete-leftmost-child   u/remove-leftmost-child)
(def  delete-rightmost-child  u/remove-rightmost-child)
(def  delete-children         u/remove-children)
(def  delete-siblings         u/remove-siblings)

(def  replace-point           u/replace-point)

(def  insert-left             u/insert-left)
(def  insert-right            u/insert-right)
(def  insert-leftmost         u/insert-leftmost)
(def  insert-rightmost        u/insert-rightmost)
(def  insert-leftmost-child   u/insert-child)
(def  insert-rightmost-child  u/append-child)
(def  insert-break-left       #(u/insert-left % (r/read-string "\n\n"))) 
(def  insert-break-right      #(u/insert-right % (r/read-string "\n\n"))) 
