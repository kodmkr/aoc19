(in-package :day03)

(defun extract (direction)
  (cons (char direction 0) (parse-integer direction :start 1)))

(defun read-input (pathstr)
  (with-open-file (s pathstr)
    (list (mapcar #'extract (cl-ppcre:split "," (read-line s nil nil)))
          (mapcar #'extract (cl-ppcre:split "," (read-line s nil nil))))))

(defun up (coord amount)
  (+ coord (* amount #c(0 1))))

(defun down (coord amount)
  (- coord (* amount #c(0 1))))

(defun right (coord amount)
  (+ coord (* amount #c(1 0))))

(defun left (coord amount)
  (- coord (* amount #c(1 0))))

(defun make-coords (path)
  (let (coords)
    (push #c(0 0) coords)
    (loop for dir in path
          collect
          (destructuring-bind (udlr . amount) dir
            (push (alexandria:switch (udlr :test #'char=)
                    (#\U (up (first coords) amount))
                    (#\D (down (first coords) amount))
                    (#\L (left (first coords) amount))
                    (#\R (right (first coords) amount)))
                  coords)))
    (nreverse coords)))

;; from mathworld
(defun intersection-p (p1 q1 p2 q2)
  (flet ((det (a b c d)
           (- (* a d) (* b c))))
    (let* ((p1-x (realpart p1)) (p1-y (imagpart p1))
           (q1-x (realpart q1)) (q1-y (imagpart q1))
           (p2-x (realpart p2)) (p2-y (imagpart p2))
           (q2-x (realpart q2)) (q2-y (imagpart q2))
           (xy-d1 (det p1-x p1-y q1-x q1-y))
           (xy-d2 (det p2-x p2-y q2-x q2-y))
           (x-d3 (det xy-d1 (- p1-x q1-x) xy-d2 (- p2-x q2-x)))
           (y-d3 (det xy-d1 (- p1-y q1-y) xy-d2 (- p2-y q2-y)))
           (xy-d (det (- p1-x q1-x) (- p1-y q1-y) (- p2-x q2-x) (- p2-y q2-y))))
      (unless (or (zerop xy-d) (zerop p1) (zerop p2))
        (let ((i-x (/ x-d3 xy-d))
              (i-y (/ y-d3 xy-d)))
          (when (and (<= (min p1-x q1-x) i-x (max p1-x q1-x)) ;; cludge
                     (<= (min p2-x q2-x) i-x (max p2-x q2-x))
                     (<= (min p1-y q1-y) i-y (max p1-y q1-y))
                     (<= (min p2-y q2-y) i-y (max p2-y q2-y)))
            (complex i-x i-y)))))))

(defun collect-intersections (dirs1 dirs2 &key weight)
  (let ((coords1 (make-coords dirs1))
        (coords2 (make-coords dirs2))
        intersections)
    (loop for (p1 q1) on coords1
          for (d1 . c1) in dirs1 sum c1 into costs1
          if (and p1 q1) do
            (loop
              for (p2 q2) on coords2
              for (d2 . c2) in dirs2 sum c2 into costs2
              if (and p2 q2) do
                (alexandria:if-let (i (intersection-p p1 q1 p2 q2))
                  (push (if weight
                            (cons i (+ (- costs1 c1)
                                       (- costs2 c2)
                                       (floor (abs (- p1 i)))
                                       (floor (abs (- p2 i)))
                                       ))
                            i)
                        intersections))))
    intersections))

(defun manhattan (complex)
  (+ (abs (realpart complex))
     (abs (imagpart complex))))

(defun sol1 ()
  (destructuring-bind (dir1 dir2)
      (read-input "./inputs/day03")
    (-<> (collect-intersections dir1 dir2)
         (mapcar #'manhattan <>)
         (sort <> #'<)
         (first <>))))

(defun sol2 ()
  (destructuring-bind (dir1 dir2)
      (read-input "./inputs/day03")
    (-<> (collect-intersections dir1 dir2 :weight t)
         (sort <> #'< :key #'cdr)
         (cdar <>))))
