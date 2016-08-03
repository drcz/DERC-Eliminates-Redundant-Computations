(use-modules (ice-9 nice-9) ; the real magic.
	     (ice-9 pretty-print))

(include "kleene-defs.scm")
(include "peval-memory-and-stuff.scm")
(include "code-massage.scm")

(define (error msg)
  (display msg)
  (newline)
  (pretty-print (get-residual-program))
  (quit))

;; some peval-specific terms:
(define (static? expr)
  "a static expression evaluates to constant"
  (or (numeral? expr)
      (T/nil? expr)
      (quote-form? expr)))

(define (static->value expr)
  "the value of a static expression"
  [assert (static? expr)]
  (if (quote-form? expr)
      (quoted-expr expr)
      expr))

(define (value->static val)
  "the static expression which evaluates to given value"
  (cond ((numeral? val) val)
	((T/nil? val) val)
	(else `(quote ,val))))


(define (plookup var env)
  "emit code -- either producing a constant, or looking up a [dynamic] variable"
  (let ((val (lookup var env)))
    (if val (value->static val)	var)))


;; ok, let's go:

(define (pevaluate expr penv prog)
;(pretty-print `(pevl ,expr ,penv))
  (cond [(or (numeral? expr) (T/nil? expr)) expr]
	[(quote-form? expr) expr]
	[(variable? expr) (plookup expr penv)]
	[(if-form? expr)
	 (match (pevaluate (if-premise expr) penv prog)
	   [() (pevaluate (if-alternative expr) penv prog)]
	   [(? static?) (pevaluate (if-conclusion expr) penv prog)]
	   [e `(if ,e ;; todo: maybe create two new functions  for each branch?
		   ,(pevaluate (if-conclusion expr) penv prog)
		   ,(pevaluate (if-alternative expr) penv prog))])]
	[(cond-form? expr)
	 (pevaluate (cond->ifs expr) penv prog)]
	[(primitive-application? expr)
	 (peval-primitive (operator expr)
			  (map (lambda (x) (pevaluate x penv prog)) (operands expr))
			  expr)]
	[(function-application? expr)
	 (peval-function (operator expr)
			 (map (lambda (x) (pevaluate x penv prog)) (operands expr))
			 prog
			 expr)]))


;; here comes some knowledge on residualizing primitive operations:
(define (peval-primitive rator rands expr)
  (match `(,rator . ,rands)
    [('+ (? numeral? e1) (? numeral? e2)) (+ e1 e2)]
    [('+ 0 e) e]
    [('+ e 0) e]
    [('+ e1 e2) `(+ ,e1 ,e2)]
    [('- (? numeral? e1) (? numeral? e2)) (- e1 e2)]
    [('- e 0) e]
    [('- e1 e2) `(- ,e1 ,e2)]
    [('* (? numeral? e1) (? numeral? e2)) (* e1 e2)]
    [('* e 0) 0]
    [('* 0 e) 0]
    [('* e 1) e]
    [('* 1 e) e]
    [('* e1 e2) `(* ,e1 ,e2)]  
    [('= (? static? e1) (? static? e2)) (if (equal? e1 e2) 'T '())]
    [('= e e) 'T] ;; sure?
    [('= e1 e2) `(= ,e1 ,e2)]
    [('< (? numeral? n) (? numeral? n)) 'T]
    [('< (? static?) (? static?)) '()]
    [('< e1 e2) `(< ,e1 ,e2)]
    [('car (? static? e)) (value->static (car (static->value e)))]
    [('car ('cons e _)) e]
    [('car e) `(car ,e)]
    [('cdr (? static? e)) (value->static (cdr (static->value e)))]
    [('cdr ('cons _ e)) e]    
    [('cdr e) `(cdr ,e)]
    [('cons (? static? e1) (? static? e2))
     (value->static (cons (static->value e1)
			  (static->value e2)))]
    [('cons e1 e2) `(cons ,e1 ,e2)]
    [('num? (? numeral?)) 'T]
    [('num? (? static?)) '()]
    [('num? e) `(num? ,e)]
    [('atom? (? static? e)) (if (pair? (static->value e)) '() 'T)]
    [('atom? e) `(atom? ,e)]
    [otherwise     
     (error `(malformed primitive application ,expr (,rator . ,rands)))]))



;; now the darkest, and most mystical part of pevaluator -- residualizing calls.
(define (peval-function fname rands prog expr)
;  (pretty-print `(pvl-fn ,fname ,rands))
  (let* ([def (assoc fname prog)]
	 [varlist (definition-varlist def)]
	 [body (definition-body def)])
    (if (= (length varlist) (length rands))
	(let* ([pseudo-env (map cons varlist rands)]
	       [static dynamic (partition (lambda ((var . expr)) (static? expr))
					  pseudo-env)]
	       [penv (map (lambda ((var . expr)) `(,var . ,(static->value expr)))
			  static)])
	  (residualize-call fname penv dynamic prog))
	(error `(wrong number of arguments to ,fname in ,expr)))))


(define (residualize-call fname penv dynamic prog)

  (define (generalize penv1 penv2)
    "find new penv on which both given match (``a most specific generalization'')"
    [assert (every (lambda (x) (member? x (map car penv2))) (map car penv1))]
    (filter-map (lambda ((var . val))
		  (and (equal? val (lookup var penv2))
		       `(,var . ,val)))
		penv1))
  
  (define ((is? x) y) (equal? x y)) ;; told ya, it's magic!
  
;[pretty-print `(r-c ,fname ,penv try?= ,(try-to-recall fname penv))]
  (match (try-to-recall fname penv)    
    ['nothing-seen-so-far
     (residualize-new-call fname penv dynamic prog)]
    [(((? (is? fname)) (? (is? penv))) resid-name resid-args resid-body)
     (call-or-inline resid-name resid-args resid-body dynamic)]
    [(((? (is? fname)) other-penv) _ _ _)
     (let* ([new-penv (generalize other-penv penv)]
	    [new-static-vars (map car new-penv)]
	    [dynamized (filter-map (lambda ((var . val))
				     (and (not (member? var new-static-vars))
					  `(,var . ,(value->static val))))
				   penv)]
	    [new-dynamic (append dynamized dynamic)])
       ;; todo: should we forget previous one and replace all calls in *mem*?!
       ;; -> not for now, I don't care it sometimes unfolds loop's first step.
       (residualize-call fname new-penv new-dynamic prog))]))


(define (call-or-inline fname args body dynamic)
  "build a residual call... or inline :)"
  ;;; TODO: beta-reduction if body is inlineable?
  (if (static? body) ;; maybe it does not need those dynamic args at all?
      body
      `(,fname . ,(map (lambda (var) (lookup var dynamic)) args))))

(define (residualize-new-call fname penv dynamic prog)
  (let* ([new-name (gensym (symbol->string fname))]
	 [new-args (map car dynamic)]
	 [placeholder `(,new-name ,new-args #f)]
	 [body (definition-body (assoc fname prog))])
    (remember-new! fname penv placeholder)
    (let* ([new-body (pevaluate body penv prog)]
	   [full-entry `(,new-name ,new-args ,new-body)])
      (remember-update! fname penv full-entry)
      (call-or-inline new-name new-args new-body dynamic))))


(define (specialize prog names values)
  (let* ([def (starter prog)]
	 [penv (map cons names values)]	 
	 [dynamic-vars (lset-difference eq? (definition-varlist def) names)]
	 [dynamic (map cons dynamic-vars dynamic-vars)])
    ;; todo sprawdzic czy names < args itp...
    (forget-everything!)
    (residualize-new-call (definition-fname def)
			  penv
			  dynamic
			  prog)
    (remove-dead-code
     (inline-linear-calls
      (get-residual-program)))))


;;;;;;;;;;;;;;;;;;;;;;;;
#;(let ((p1 '[(mul (x y) (* x y))]))
  (forget-everything!)
  (residualize-new-call 'mul '([x . 3]) '([y . (+ z z)]) p1)
  (pretty-print (get-residual-program)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([prog (read)]
      [names (read)]
      [vals (read)])
  [assert (program? prog)]
  (pretty-print (specialize prog names vals)))

  
