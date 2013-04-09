(ns tailrecursion.stoke.mode.normal
  (:require
    [tailrecursion.stoke.term :as t]
    [tailrecursion.stoke.cmd  :as c]))

(def move-left           (t/multi c/move-left))
(def move-leftmost       (t/once c/move-leftmost))
(def move-right          (t/multi c/move-right))
(def move-rightmost      (t/once c/move-rightmost))
(def move-up             (t/multi c/move-up))
(def move-down           (t/multi c/move-down))
(def move-next           (t/multi c/move-next))
(def move-prev           (t/multi c/move-prev))

(def insert-break-left   (t/multi c/insert-break-left))
(def insert-break-right  (t/multi c/insert-break-right))

(def delete-point        (t/multi c/delete-point))
(def delete-left         (t/multi c/delete-left))
(def delete-lefts        (t/once c/delete-lefts))
(def delete-right        (t/multi c/delete-right))
(def delete-rights       (t/once c/delete-rights))
(def delete-siblings     (t/once c/delete-siblings))
