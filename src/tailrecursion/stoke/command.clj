(ns tailrecursion.stoke.command
  (:require
    [clojure.zip :as z]
    [tailrecursion.stoke.edit :refer [point]]))

(defn- guard! [f & args]
  (try (apply f args) (catch Throwable e)))

(defn default [] nil)
(defn quit    [] :quit)
(defn left    [] (guard! swap! point z/left))
(defn right   [] (guard! swap! point z/right))
(defn up      [] (guard! swap! point z/up))
(defn down    [] (guard! swap! point z/down))
