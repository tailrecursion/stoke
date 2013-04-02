(ns tailrecursion.stoke.pp
  (:require
    [bbloom.fipp.printer        :as p :refer [defprinter]]
    [tailrecursion.stoke.reader :as r]))

(def cat      (partial interpose :line))
(def delims   #(:delims (meta %)))
(def prefix   #(:prefix (meta %)))


(comment
  
  (meta (r/read-string "{:foo\n \"bar\"}")) 
  )
