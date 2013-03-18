(ns tailrecursion.stoke.vikeys
  (:require
    [tailrecursion.stoke.command :as c]))

(defmulti command identity)

(defmethod command :default     [x] (c/default))
(defmethod command [:normal \q] [x] (c/quit))
(defmethod command [:normal \h] [x] (c/left))
(defmethod command [:normal \l] [x] (c/right))
(defmethod command [:normal \j] [x] (c/down))
(defmethod command [:normal \k] [x] (c/up))
