;;; the second-best unit-test "library" in the world:
(use-modules (ice-9 pretty-print))

(define-macro (assert p)
  `(if ,p
       #t
       (begin (display '(assertion ,p failed!))
              (newline)
              #f)))

(define-macro (assert-eq? p v)
  `(if (equal? ,p ,v)
       #t
       (begin (pretty-print (list 'assert-eq? ',p ,v 'failed!))
              (pretty-print (list 'because ',p '=> ,p))
              (newline)
              #f)))

;;; eg:
;(assert (= 2 2))
;(assert-eq? (+ 3 3) (* 3 2))

;;; failed test pretty-prints what went wrong, eg
;(assert (= 2 3))

