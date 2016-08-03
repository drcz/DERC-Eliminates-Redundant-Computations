(use-modules (ice-9 match) #;for-eval-primitive
	     (ice-9 pretty-print) #;for-main-call)

(include "kleene-defs.scm")

(define (evaluate expr env prog)
  (cond [(or (numeral? expr) (T/nil? expr)) expr]
	[(quote-form? expr) (quoted-expr expr)]
	[(variable? expr)	
	 (or (lookup expr env) (error `(unbound symbol ,expr)))]
	[(if-form? expr)
	 (if (eq? '() (evaluate (if-premise expr) env prog))
	     (evaluate (if-alternative expr) env prog)
	     (evaluate (if-conclusion expr) env prog))]
	[(cond-form? expr)
	 (evaluate (cond->ifs expr) env prog)]
	[(or (primitive-application? expr)
	     (function-application? expr))
	 (let ([rator (operator expr)]
	       [rands (map (lambda (x) (evaluate x env prog)) (operands expr))])
	   (if (primitive-application? expr)
	       (eval-primitive rator rands expr)
	       (eval-function rator rands prog expr)))]))


(define (eval-primitive rator rands expr)
  (match `(,rator . ,rands)
    [('+ (? numeral? e1) (? numeral? e2)) (+ e1 e2)]
    [('- (? numeral? e1) (? numeral? e2)) (- e1 e2)]
    [('* (? numeral? e1) (? numeral? e2)) (* e1 e2)]
    [('= e1 e2) (if (equal? e1 e2) 'T '())]
    [('< (? numeral? e1) (? numeral? e2)) (if (< e1 e2) 'T '())]
    [('car (? pair? e)) (car e)]
    [('cdr (? pair? e)) (cdr e)]
    [('cons e1 e2) (cons e1 e2)]
    [('num? (? numeral?)) 'T]
    [('num? _) '()]
    [('atom? (? pair?)) '()]
    [('atom? _) 'T]
    [otherwise
     (error `(malformed primitive application ,expr (,rator . ,rands)))]))


(define (eval-function fname rands prog expr)
  (let ([def (assoc fname prog)])
    (if def
	(let ([body (definition-body def)]
	      [args (definition-varlist def)])
	  (if (= (length args) (length rands))
	      (evaluate body (map cons args rands) prog)
	      (error `(wrong number of arguments to ,fname in ,expr))))
	(error `(undefined function ,fname)))))


;;; ``everybody's in the place -- let's go''
(let ([prog (read)]
      [inputs (read)])
  [assert (program? prog)]
  [assert (list? inputs)]
  ;;; the result of prog wrt inputs is the application of first function
  ;;; in prog (i.e. starter) with inputs as operand values.
  (pretty-print
   (eval-function (definition-fname (starter prog)) inputs prog 'the-main-call)))
