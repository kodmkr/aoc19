(in-package :day08)

(defparameter +format+ (list 6 25))

(defun mk-layer ()
  (make-array +format+))

(defun read-input (pathstr)
  (with-open-file (s pathstr)
    (let (layers)
      (loop
        (let ((layer (mk-layer)))
          (loop for x below (first +format+) do
            (loop for y below (second +format+) do
              (let ((c (read-char s nil nil)))
                (if (not c)
                    (return-from read-input (nreverse layers))
                    (setf (aref layer x y) (- (char-code c) (char-code #\0)))))))
          (push layer layers))))))

(defun count-0-digits-in-layer (layer)
  (loop for i below (array-dimension layer 0)
        sum (loop for j below (array-dimension layer 1)
                  if (zerop (aref layer i j)) count 1)))

(defun sort-layers-by-0 (layers)
  (sort (copy-seq layers) #'< :key #'count-0-digits-in-layer))

(defun multiply-ones-n-twos (layer-with-least-zeros)
  (let ((ones 0)
        (twos 0))
    (loop for i below (array-dimension layer-with-least-zeros 0) do
      (loop for j below (array-dimension layer-with-least-zeros 1) do
        (cond ((= (aref layer-with-least-zeros i j) 1)
               (incf ones))
              ((= (aref layer-with-least-zeros i j) 2)
               (incf twos)))))
    (* ones twos)))

(defun sol1 ()
  (-<> (read-input "./inputs/day08")
       (sort-layers-by-0 <>)
       (first <>)
       (multiply-ones-n-twos <>)))

(defun mk-image (&optional image-1 image-2)
  (if (and (null image-1) (null image-2))
      nil
      (let* ((dim-1 (array-dimension image-1 0))
             (dim-2 (array-dimension image-1 1))
             (res (make-array (list dim-1 dim-2) :initial-element 2)))
        (loop for i below dim-1 do
          (loop for j below dim-2 do
            (anaphora:slet (aref image-1 i j)
              (case anaphora:it
                ((0 1) (setf (aref res i j) anaphora:it))
                (otherwise (setf (aref res i j) (aref image-2 i j)))))))
        res)))

(defun sol2 ()
  (-<> (read-input "./inputs/day08")
       (reduce #'mk-image <>)))
