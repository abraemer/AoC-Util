;;;; aoc-util.asd

(asdf:defsystem #:aoc-util
  :description "Various helpers for Advent of Code."
  :author "NobodysHero"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:drakma #:cl-heap)
  :components ((:file "package")
               (:file "utilities")))
