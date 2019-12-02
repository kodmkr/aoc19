;;;; aoc19.asd

(asdf:defsystem #:aoc19
  :description "Advent of Code 2019"
  :depends-on ("cl-ppcre" "arrows" "alexandria" "anaphora")
  :serial t
  :components ((:file "package")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "day04")
               ))
