;;;; aoc19.asd

(asdf:defsystem #:aoc19
  :description "Advent of Code 2019"
  :depends-on ("cl-ppcre" "arrows" "alexandria")
  :serial t
  :components ((:file "package")
               (:file "day01")
               (:file "day02")
               ))
