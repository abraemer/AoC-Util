;;;; utilities.lisp

(in-package #:aoc-util)

(defun current-year ()
  (nth-value 5 (get-decoded-time)))

(defparameter *year* (write-to-string (current-year)))

;(defconstant +path+ "D:/Daten/lisp/advent-of-code-2018/inputs/")
(defparameter +path+ "~/Downloads/aoc")

(defun download-puzzle-input (day file &optional (year (current-year)))
  (let ((session-file (puzzlepath "session.txt"))
        (cookie-jar (make-instance 'drakma:cookie-jar)))
    (unless (probe-file session-file)
      (update-session))
    (push (make-instance 'drakma:cookie
                         :name "session" :domain "adventofcode.com"
                         :value 
                         (with-open-file (session-in session-file)
                           (read-line session-in)))
          (drakma:cookie-jar-cookies cookie-jar))
    (destructuring-bind (body code . stuff)
        (multiple-value-list
         (drakma:http-request (format nil "https://adventofcode.com/~d/day/~d/input" year day)
                              :cookie-jar cookie-jar))
      (unless (= code 200)
        (format t "Something went wrong! Return status code: ~a (~a)~%" code (first (last stuff)))
        (return-from download-puzzle-input))
      (with-open-file (out file :direction :output :if-does-not-exist :create :if-exists :overwrite)
        (format out "~a" body))
      (format t "Successfully downloaded input and wrote to: ~a~%" file))))

(defun update-session ()
  (let ((session-file (puzzlepath "session.txt")))
    (format t "Session id for the cookie please:~%")
    (with-open-file (session-out session-file :direction :output
                                              :if-does-not-exist :create
                                              :if-exists :overwrite)
      (format session-out (read-line *standard-input*)))))

(defun puzzlefile (day &optional (year (current-year)))
  (let ((file (puzzlepath (format nil "input~2,'0d.txt" day))))
    (or (and (probe-file file) file)
        (download-puzzle-input day file year)
        file)))

(defun puzzlepath (file)
  (concatenate 'string +path+ *year* "/" file))

(defmacro loop-line-by-line (file &body body)
  (let ((in (gensym)))
    `(with-open-file (,in ,file)
       (when ,in
         (loop for line = (read-line ,in nil)
               while line
               ,@body)))))

(defun read-puzzlefile (day &optional (year (current-year)))
  (loop-line-by-line (puzzlefile day year)
    collect line)) 

(defun split-seq (seq denom)
  (labels
      ((rec (start accum)
         (let ((index (position denom seq :start start)))
           (if index
               (rec (+ 1 index) (cons (subseq seq start index) accum))
               (cons (subseq seq start) accum)))))
    (nreverse (rec 0 nil)))) 

(defun permutations (list)
  "Returns a list of all permutations of list"
  (if (null list)
      (list nil)
      (loop
	 :for element :in list
	 :nconc (mapcar (lambda (subperm) (cons element subperm))
			(permutations (remove element list))))))

(defun make-circular! (list)
  "Takes a list and makes it circular"
  (setf (cdr (last list)) list))

(defun hash-keys (hashtable)
  (loop :for key :being :the :hash-keys :of hashtable
        :collect key))

(defun max-key (hashtable &key (test #'>) (accessor #'identity))
  (let ((keys (hash-keys hashtable)))
    (loop
      :with max-key := (first keys)
      :with max-val := (gethash max-key hashtable)
      :for key :in (rest keys)
      :for val := (gethash key hashtable)
      :when (funcall test
                     (funcall accessor val)
                     (funcall accessor max-val))
      :do (setf max-key key max-val val)
      :finally (return (values max-key max-val)))))

(defun max-index (vector)
  (loop
    :with max-ind := 0
    :with max-val := (aref vector 0)
    :for index :from 1 :below (length vector)
    :for value := (aref vector index)
    :when (> value max-val) :do (setf max-ind index max-val value)
    :finally (return (values max-ind max-val))))

(defun build-set (seq &key (test 'eql) (key #'identity))
  (let ((set (make-hash-table :test test)))
    (map nil (lambda (k) (setf (gethash (funcall key k) set) t)) seq)
    set))

(defun extract-integers (str)
  (nreverse
   (let ((list nil))
     (ppcre:do-register-groups ((#'parse-integer int))
         ("(-?\\d+)" str)
       (push int list))
     list)))

(defun dijkstra (start end distance-function neighbour-function &key (test 'eql))
  "Finds the shortest path from the start node to the end node. The neighbour-function should return a proper list of nodes adjacent to the given node. The distance-function should return a non-negative distance between the given node."
  (loop :with open := (make-instance 'cl-heap:fibonacci-heap :key #'first)
        :with closed := (make-hash-table :test test)
        :initially (dolist (n (funcall neighbour-function start))
                     (cl-heap:add-to-heap open
                                          (list (funcall distance-function start n) n start)))
                   (setf (gethash start closed) (cons 0 nil))
        :while (> (cl-heap:heap-size open) 0)
        :until (gethash end closed nil)
        :for (distance node prior) := (cl-heap:pop-heap open)
        ;; explore the node if not explored yet
        :unless (gethash node closed nil) :do
        (setf (gethash node closed) (cons distance prior))
        (dolist (neighbour (funcall neighbour-function node))
          (unless (gethash neighbour closed nil)
            (cl-heap:add-to-heap open (list (+ distance (funcall distance-function node neighbour))
                                            neighbour node))))
        ;; reconstruct the path
        :finally (loop :with path := nil
                       :for at := end :then (rest (gethash at closed)) :while at
                       :do (push at path)
                       :finally (return-from dijkstra (values path (first (gethash end closed)))))))
