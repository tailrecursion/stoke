(ns tailrecursion.stoke.mode.undo
  (:require
    [tailrecursion.stoke.edit :as e]
    [tailrecursion.stoke.cmd  :as c]
    [tailrecursion.stoke.term :as t]))

(def undo-left      (t/multi e/undo c/move-left))
(def undo-leftmost  (t/multi e/undo c/move-leftmost))
(def undo-right     (t/multi e/undo c/move-right))
(def undo-rightmost (t/multi e/undo c/move-rightmost))
(def undo-up        (t/multi e/undo c/move-up))
(def undo-down      (t/multi e/undo c/move-down))
(def undo-next      (t/multi e/undo c/move-next))
(def undo-prev      (t/multi e/undo c/move-prev))

