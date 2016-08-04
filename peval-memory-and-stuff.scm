;;;;; remembering, recalling, similarity (a whistle) and generalization...

;;; quite ugly, but who cares?
(define *mem* '())

(define (forget-everything!) (set! *mem* '()))

(define (get-residual-program) (reverse (map cdr *mem*)))

(define (try-to-recall fname penv)
  ;;; todo: try to assoc first maybe?
  ;;; or should we take them all, sort with whistle and pick the greatest one??
  ;;; (if there is the same one already, it will be ``the whistlest'', as every
  ;;; generalization is embedded in ``the original''...)
  (look-for-whistle `(,fname ,penv)))

(define (remember-new! fname penv entry)
;  (pretty-print `(rem-new! (,fname ,penv)))
  (set! *mem* `([(,fname ,penv) . ,entry] . ,*mem*)))

(define (remember-update! fname penv entry)
;  (pretty-print `(rem-upd! (,fname ,penv)))
  (set! *mem* (update `(,fname ,penv) entry *mem*)))

(define (atom? x) (not (pair? x))) ;; :D

;;; the famous ``russian whistle'' :)
(define (whistle? e1 e2)
  "e1 is [homeomorphically] embedded in e2"
;  (pretty-print `(<? ,e1 ,e2))
  (match `(,e1 ,e2)
    [(e e) #t]
    [(() e) (null? e)] ;;; !!!!!!!!
#;    [((? number? n1) (? number? n2)) #t]
#;    [((? number? n1) (? number? n2))
     (or (< 0 n1 n2)
	 (> 0 n1 n2))]
    ;;; TODO: when working on 2level interpreter add cases for meta-vars
    ;;; so that each two do whistle, disregarding their "numbers"...
    [((h . t) (? atom?)) #f]
    [((h1 . t1) (h2 . t2))
     (or (and (whistle? h1 h2) (whistle? t1 t2))
	 (whistle? `(,h1 . ,t1) h2)
	 (whistle? `(,h1 . ,t1) t2))]
    [(e (h . t)) (or (whistle? e h) (whistle? e t))]
    [otherwise #f]))

;(e.g. (not (whistle? '(q w e) '(w e))))
;(e.g. (whistle? '(w e) '(q w e)))
;;;(e.g. (whistle? 3 5)) ;; because [in unary] (whistle? '(I I I) '(I I I I I))

(define (signatures-whistle? (name1 static1) (name2 static2))
  "when two applications are ''dangerously similar''"
  (and (eq? name1 name2)
       (= #;<= (length static1) (length static2))
       (every (lambda ((var . val))
		;;;;;; VEEEEEEERY DIRTY HACKS....
		(and (not (and (or (and (eq? name1 'eval)
					(eq? var 'expr))
				   (and (eq? name1 'evlis)
					(eq? var 'exprs))
				   (and (eq? name1 'apply)
					(eq? var 'rator)))
			       (not (eq? val (lookup var static2)))))
		     ;;;;;;;;;;; \VEEEEEEERY DIRTY HACKS
		     (whistle? val (lookup var static2))))
	      static1)))

(define (look-for-whistle call)
  "is this call similar to anything we saw before? of so, bring this memory..."
  (let* ([all-whistling-projections
	  (filter (lambda ((signature . entry))
		    (signatures-whistle? signature call))
		  *mem*)]
	 [from-most-specific-to-most-generic
	  (sort all-whistling-projections
		(lambda ((s1 . e1) (s2 . e2)) (signatures-whistle? s1 s2)))])
#;    (begin
      (display `(spec->gen 4 ,call)) (newline)
      (map (lambda (x) (display x) (newline)) from-most-specific-to-most-generic)
      (newline))  
    (if (null? from-most-specific-to-most-generic)
	'nothing-seen-so-far
	(first from-most-specific-to-most-generic)))
#;  (fold-right (lambda (h t) (if (signatures-whistle? (car h) call) h t))
	      'nothing-seen-so-far
	      (reverse *mem*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
