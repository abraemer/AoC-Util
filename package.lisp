;;;; package.lisp

(defpackage #:aoc-util
  (:use #:cl)
  (:export :read-puzzlefile
	   :puzzlefile
	   :split-seq
	   :hash-keys
	   :loop-line-by-line :line
	   :make-circular!
	   :permutations
	   :max-key
	   :max-vector
	   :build-set
	   :extract-integers
	   :dijkstra))
