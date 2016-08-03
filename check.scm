(include "kleene-defs.scm")

(let ((prg (read)))
  (map (lambda (d) (display `(,(definition? d) -> ,d)) (newline)) prg))
