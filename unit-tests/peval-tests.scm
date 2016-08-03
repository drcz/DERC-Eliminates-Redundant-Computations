#!/usr/bin/guile \
--no-auto-compile -s
!#

(include "test-macros.scm")
(include "../partial-evaluator.scm")

(define (test-peval-primitive)
  (and
   ;;; basic cases:
   [assert-eq? (peval-primitive '+ '[2 3] 'test) 5]
   [assert-eq? (peval-primitive '+ '[a 3] 'test) '(+ a 3)]
   [assert-eq? (peval-primitive '+ '[a (- b 1)] 'test) '(+ a (- b 1))]   
   [assert-eq? (peval-primitive 'car '['(q w e)] 'test) ''q]
   [assert-eq? (peval-primitive 'car '[a] 'test) '(car a)]
   [assert-eq? (peval-primitive 'cdr '[(cons 2 3)] 'test) 3]
   [assert-eq? (peval-primitive 'cdr '[x] 'test) '(cdr x)]
   [assert-eq? (peval-primitive 'cdr '[x] 'test) '(cdr x)]
   [assert-eq? (peval-primitive '= '[2 3] 'test) '()]
   [assert-eq? (peval-primitive '= '[3 3] 'test) 'T]
   [assert-eq? (peval-primitive '= '[a 3] 'test) '(= a 3)]
   [assert-eq? (peval-primitive '= '[a (+ b 2)] 'test) '(= a (+ b 2))]
   [assert-eq? (peval-primitive 'num? '[3] 'test)  'T]
   [assert-eq? (peval-primitive 'num? '['(23)] 'test) '()]
   [assert-eq? (peval-primitive 'num? '[x] 'test) '(num? x)]
   ;; (...)
   ;;; some cheap tricks:
   [assert-eq? (peval-primitive '+ '(a 0) 'test) 'a]
   [assert-eq? (peval-primitive '- '((+ a b) 0) 'test) '(+ a b)]
   [assert-eq? (peval-primitive '* '((+ a b) 0) 'test) 0]
   [assert-eq? (peval-primitive '* '((+ a b) 1) 'test) '(+ a b)]
   [assert-eq? (peval-primitive 'car '[(cons a b)] 'test) 'a]   
   [assert-eq? (peval-primitive 'cdr '[(cons a b)] 'test) 'b]
   ))

;; todo!!!
#;[begin
  (pevaluate '(if x (+ y x) x) '((y . 3)) '())
  (pevaluate '(cons (+ x x) (quote x)) '((x . 3)) '())
  (pevaluate '(car (quote (quote x))) '() '())
  (pevaluate '(car (cons (+ x y) y)) '((y . 23)) '()) ]

(newline)
(if [and (test-peval-primitive)
       ;; ...
	 ]
    (display "all tests passed.")
    (display "there were some failures."))
(newline)



   
   
