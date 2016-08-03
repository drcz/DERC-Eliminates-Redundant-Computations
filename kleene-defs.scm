(use-modules (srfi srfi-1))

;;; if anything goes wrong, I'll kill myself!
(define (error msg) (display "ERROR! ") (display msg) (newline) (exit))

;;; Panicz Godek way -- [for now] this is for you, not the machine:
(define-macro (e.g. e ===> v) `(begin ))

;;; since out assertions are very simple, we can impelemnt this
(define-macro (assert pred)
  `(if (not ,pred)
       (error '(assertion ,pred failed!))))

;;; + some crap I like:
(define (lookup sym env)
  (let ([val (assoc sym env)])
    (if val (cdr val) #f)))

(define (update sym val env)
  `([,sym . ,val] . ,(alist-delete sym env)))

(define (member? x xs) (if (member x xs) #t #f)) ;; :D

(define (list? x) (or (null? x) (and (pair? x) (list? (cdr x)))))

(define (rest x)
  [assert (and (list? x) (not (null? x)))]
  (cdr x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (program? x)
  (and (list? x)
       (> (length x) 0)
       (every definition? x)))
;;; todo: perhaps we should collect all calls and check fnames and arities too??

(e.g. (program? '[(hello (n m) (cons (factorial n) (factorial m)))
		  (factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))])
      ===> #t)

(define (starter prg) [assert (program? prg)] (first prg))

(define (definition? x)

  (define (varlist? x)
    (and (list? x)
	 (every variable? x)
	 (all-are-distinct? x)))
  
  (define (all-are-distinct? x)
    (or (null? x)
	(and (not (member? (first x) (rest x)))
	     (all-are-distinct? (rest x)))))
  
  (and (list? x)
       (= (length x) 3)
       (fname? (first x))
       (varlist? (second x))
       (expression? (third x))
       (every (lambda (var) (member? var (second x)))
	      (free-variables (third x)))))

(define (definition-fname def) [assert (definition? def)] (first def))
(define (definition-varlist def) [assert (definition? def)] (second def))
(define (definition-body def) [assert (definition? def)] (third def))
       
(define (expression? x)
  (or (numeral? x)
      (T/nil? x)
      (variable? x)
      (quote-form? x)
      (if-form? x)
      (cond-form? x)
      (primitive-application? x)
      (function-application? x)))

(define (free-variables expr)
  [assert (expression? expr)]
  (cond [(or (numeral? expr)
	     (T/nil? expr)
	     (quote-form? expr))
	 '()]
	[(variable? expr)
	 (list expr)]
	[(if-form? expr)
	 (delete-duplicates (append (free-variables (if-premise expr))
				    (free-variables (if-conclusion expr))
				    (free-variables (if-alternative expr))))]
	[(cond-form? expr)
	 (free-variables (cond->ifs expr))] ;; :)
	[(or (primitive-application? expr)
	     (function-application? expr))
	 (delete-duplicates (append-map free-variables (operands expr)))]))

(define (numeral? x) (number? x))
(define (T/nil? x) (or (eq? x '()) (eq? x 'T)))
(define (primitive? x) (or (unary-primitive? x) (binary-primitive? x)))
(define (unary-primitive? x) (member? x '(car cdr num? atom?)))
(define (binary-primitive? x) (member? x '(cons + - * = <)))
(define (variable? x) (and (symbol? x)
			   (not (primitive? x))
			   (not (T/nil? x))
			   (not (member x '(if cond quote)))))
(define (fname? x) (variable? x)) ;; sorry ;)

(define (quote-form? x)
  (and (list? x)
       (= (length x) 2)
       (eq? (first x) 'quote)))

(define (quoted-expr expr) (assert (quote-form? expr)) (second expr))

(define (if-form? x)
  (and (list? x)
       (= (length x) 4)
       (eq? (first x) 'if)
       (expression? (second x))
       (expression? (third x))
       (expression? (fourth x))))

(define (if-premise expr) [assert (if-form? expr)] (second expr))
(define (if-conclusion expr) [assert (if-form? expr)] (third expr))
(define (if-alternative expr) [assert (if-form? expr)] (fourth expr))

(define (cond-form? x)
  (define (cond-clause? x)
    (and (list? x)
	 (= (length x) 2)
	 (expression? (first x))
	 (expression? (second x))))
  (and (list? x)
       (> (length x) 1)
       (eq? (first x) 'cond)
       (every cond-clause? (rest x))))

(define (cond-cases expr) [assert (cond-form? expr)] (rest expr))

;;; oops, concrete syntax for if-form! but is it worth to do otherwise? -> nope.
(define (cond->ifs expr)
  [assert (cond-form? expr)]
  (fold-right (lambda (clause remaning)
		`(if ,(first clause) ,(second clause) ,remaning))
	      '()
	      (cond-cases expr)))

(define (primitive-application? x)
  (and (list? x)
       (or (and (= (length x) 2)
		(unary-primitive? (first x))
		(expression? (second x)))
	   (and (= (length x) 3)
		(binary-primitive? (first x))
		(expression? (second x))
		(expression? (third x))))))

;;; mind that for the sake of generality we don't check whether the fname
;;; is defined, nor whether the number of arguments matches the definition.
;;; this will be checked by the evaluator.
(define (function-application? x)
  (and (list? x)
       (> (length x) 0)
       (fname? (first x))
       (every expression? (rest x))))

(define (operator expr)
  [assert (or (primitive-application? expr) (function-application? expr))]
  (first expr))

(define (operands expr)
  [assert (or (primitive-application? expr) (function-application? expr))]
  (rest expr))


