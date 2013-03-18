(ns tailrecursion.stoke.vikeys
  (:require
    [tailrecursion.stoke.command :as c]))

(defmulti command identity)

(defmethod command :default [x] (c/default))
(defmethod command \q [x] (c/quit))
(defmethod command \h [x] (c/left))
(defmethod command \l [x] (c/right))
(defmethod command \j [x] (c/down))
(defmethod command \k [x] (c/up))
