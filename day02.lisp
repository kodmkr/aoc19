(in-package :day02)

(defun read-input (pathstr)
  (with-open-file (s pathstr)
    (read-line s nil nil)))

(defun mk-array (inp)
  (let* ((int-list (loop for num in (cl-ppcre:split "," inp) collect (parse-integer num)))
         (len (length int-list)))
  (make-array len :element-type 'integer :initial-contents int-list)))

(defun run (ary &key noun verb)
  (setf (aref ary 1) noun)
  (setf (aref ary 2) verb)
  (macrolet ((access (index) ;; ARY is hardcoded
               `(aref ary (aref ary ,index))))
    (loop with i = 0 do
         (case (aref ary i)
           (1 (setf (access (+ i 3))
                    (+ (access (+ i 1))
                       (access (+ i 2))))
              (incf i 4))
           (2 (setf (access (+ i 3))
                    (* (access (+ i 1))
                       (access (+ i 2))))
              (incf i 4))
           (99 (return-from run (aref ary 0)))))))

(defun brute-force (int-program target)
  (loop for noun from 0 to 99 do
       (loop for verb from 0 to 99 do
            (when (= (run (copy-seq int-program) :noun noun :verb verb) target)
              (return-from brute-force (+ (* 100 noun) verb))))))

(defun sol1 ()
  (-<> (read-input "./inputs/day02")
       (mk-array <>)
       (run <> :noun 12 :verb 2)))

(defun sol2 ()
  (-<> (read-input "./inputs/day02")
       (mk-array <>)
       (brute-force <> 19690720)))
