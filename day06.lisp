(in-package :day06)

(defun read-input (pathstr)
  (with-open-file (s pathstr)
    (let ((g (make-hash-table :test #'equal))
          (rev-g (make-hash-table :test #'equal)))
      (loop for line = (read-line s nil nil) while line
            do (destructuring-bind (orbited orbiter)
                   (cl-ppcre:split #\) line)
                 (push orbiter (gethash orbited g nil))
                 (setf (gethash orbiter rev-g) orbited)))
      (values g rev-g))))

(defun %count-orbits (name relations counts)
  (let ((nexts (gethash name relations)))
    (loop for next in nexts do
      (setf (gethash next counts 0) (1+ (gethash name counts 0)))
      (%count-orbits next relations counts))))

(defun count-orbits (g)
  (let ((counts (make-hash-table :test #'equal)))
    (%count-orbits "COM" g counts)
    counts))

(defun sum-orbits (counts)
  (loop for v being the hash-values of counts sum v))

(defun sol1 ()
  (-<> (read-input "./tests/day06_2")
       (count-orbits <>)
       (sum-orbits <>)))

(defun find-path (from rev-g path)
  (let ((curr (gethash from rev-g)))
    (cond ((string= curr "COM")
           (cons "COM" path))
          (t (find-path curr rev-g (cons curr path))))))

(defun find-ancestor (path1 path2)
  (loop for (e1 f1) on path1
        for (e2 f2) on path2
        if (string/= f1 f2) do
          (return e1)))

(defun sol2 ()
  (declare (optimize (debug 3)))
  (multiple-value-bind (g rev-g)
      (read-input "./inputs/day06")
    (let* ((path-you (find-path "YOU" rev-g nil))
           (path-san (find-path "SAN" rev-g nil))
           (counts (count-orbits g))
           (common-ancestor (find-ancestor path-you path-san))
           (orbit-you (gethash "YOU" counts))
           (orbit-san (gethash "SAN" counts))
           (orbit-anc (gethash common-ancestor counts)))
      (- (+ (abs (- orbit-anc orbit-you))
            (abs (- orbit-anc orbit-san)))
         2))))
