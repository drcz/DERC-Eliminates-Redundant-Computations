;;;;; remembering, recalling, similarity (a whistle) and generalization...

;;; quite ugly, but who cares?
(define *mem* '())

(define (forget-everything!)
  (set! *mem* '()))

(define (get-residual-program) (map cdr *mem*))

(define (try-to-recall fname penv)
  ;;; todo: try to assoc first maybe?
  (look-for-whistle `(,fname ,penv)))

(define (remember-new! fname penv entry)
  (set! *mem* (update `(,fname ,penv) entry *mem*)))

(define (remember-update! fname penv entry)
  (set! *mem* (update `(,fname ,penv) entry *mem*)))

;;; the famous russian whistle.
(define (whistle? e1 e2)
  "e1 is [homeomorphically] embedded in e2"
  (match `(,e1 ,e2)
    [(e e) #t]
;    [((? number? n1) (? number? n2)) #t]
    [((? number? n1) (? number? n2))
     (or (< 0 n1 n2)
	 (> 0 n1 n2))]    
    ;;; todo: perhaps 0<n1<n2 and |n1-n2|<K is safe...?
    ;;; but then, when working on 2level interpreter add cases for meta-vars
    ;;; so that each two do whistle, disregarding their "numbers"...
    [(e (h . t)) (or (whistle? e h) (whistle? e t))]
    [otherwise #f]))

;(e.g. (not (whistle? '(q w e) '(w e))))
;(e.g. (whistle? '(w e) '(q w e)))
;(e.g. (whistle? 3 5)) ;; because [in unary] (whistle? '(I I I) '(I I I I I))

(define (signatures-whistle? (name1 static1) (name2 static2))
  "when two applications are ''dangerously similar''"
  (and (eq? name1 name2)
       (<= (length static1) (length static2))
       (every (lambda ((var . val)) (whistle? val (lookup var static2))) static1)))

;(signatures-whistle? '(num ((acc))) '(num ()))

(define (look-for-whistle call)
  "is this call similar to anything we saw before? of so, bring this memory..."
  (fold-right (lambda (h t) (if (signatures-whistle? (car h) call) h t))
	      'nothing-seen-so-far
	      *mem*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
