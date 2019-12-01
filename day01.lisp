(in-package :day01)

(defun read-input (pathstr)
  (with-open-file (s pathstr)
    (loop for line = (read-line s nil nil) while line
       collect (parse-integer line))))

(defun calc (v)
  (- (floor v 3) 2))

(defun repeated-calc (v)
  (let ((curr v)
        (result 0))
    (loop do
         (setf curr (calc curr))
         (when (minusp curr)
           (return-from repeated-calc result))
         (incf result curr))))

(defun sol1 ()
  (-<> (read-input "./inputs/day01")
       (mapcar #'calc <>)
       (reduce #'+ <>)))

(defun sol2 ()
  (-<> (read-input "./inputs/day01")
       (mapcar #'repeated-calc <>)
       (reduce #'+ <>)))
