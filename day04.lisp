(in-package :day04)

(defparameter +input+ "193651-649729")

(defun extract-numbers (input)
  (let ((dash-position (position #\- input)))
    (list (map 'vector (lambda (x)
                         (- (char-code x) (char-code #\0)))
               (reverse (subseq input 0 dash-position)))
          (map 'vector (lambda (x)
                         (- (char-code x) (char-code #\0)))
               (reverse (subseq input (1+ dash-position)))))))

(defun incfa (ary)
  (let ((carry 0))
    (anaphora:slet (aref ary 0)
      (incf anaphora:it)
      (cond ((< anaphora:it 10)
             (return-from incfa ary))
            (t (setf carry 1
                     anaphora:it 0)
               (loop for i from 1 below (array-dimension ary 0) do
                 (anaphora:slet (aref ary i)
                   (setf anaphora:it (+ anaphora:it carry))
                   (if (< anaphora:it 10)
                       (return-from incfa ary)
                       (setf anaphora:it 0)))))))))

(defun decreasing-p (ary) ;; due to reversing the numbers
  (let ((dim (array-dimension ary 0)))
    (loop for i from 1 below dim
          if (< (aref ary (1- i)) (aref ary i)) do
             (return-from decreasing-p nil))
    t))

(defun some-adjacent-same-p (ary)
  (loop for i from 0 below (1- (array-dimension ary 0))
        if (= (aref ary i) (aref ary (1+ i))) do
          (return-from some-adjacent-same-p t)))

(defun count-pws (from to &key adjacency)
  (loop while (not (equalp from to))
        if (and (decreasing-p from)
                (funcall adjacency from))
          sum 1 into num-pws
        do
           (incfa from)
        finally (return-from count-pws num-pws)))

(defun sol1 ()
  (destructuring-bind (from to)
      (extract-numbers +input+)
    (count-pws from to :adjacency #'some-adjacent-same-p)))

;; should probably be moved to support library or smth...
(defun rle (ary)
  (let ((len (length ary))
        (res nil))
    (loop
      for i = 0 then j
      for j = (position-if (lambda (d) (/= d (aref ary i))) ary :start i)
      while j do
        (push (cons (aref ary i) (- j i)) res)
      finally
         (push (cons (aref ary i) (- len i)) res))
    (nreverse res)))

(defun some-adjacent-same-no-group-p (ary)
  (let ((rle (rle ary)))
    (some (lambda (c)
            (= (cdr c) 2))
          rle)))

(defun sol2 ()
  (destructuring-bind (from to)
      (extract-numbers +input+)
    (count-pws from to :adjacency #'some-adjacent-same-no-group-p)))
