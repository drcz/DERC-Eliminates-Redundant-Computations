;;; inlining and dead code removal stuff...


;;; fast'n'dirty -- todo: rewrite using abstract defs [?]
(define (gather-applied-fnames expr)
  [assert (expression? expr)]  
  (delete-duplicates
   (let gather ((expr expr))
     (match expr
       [((? fname? f) . rands)
	`(,f . ,(append-map gather rands))]
       [((? primitive?) . rands)
	(append-map gather rands)]
       [('if p c a) (append (gather p) (gather c) (gather a))]
       [otherwise '()]))))

#;(gather-applied-fnames '(f (g x)
			   xxx
			   (+ (a x) (if (a x) (b x) (a x)))
			   (h 3)
			   (quote dupa)))

(define (remove-dead-code prog)
  (let purge ([pend (list (definition-body (starter prog)))]
	      [res (list (definition-fname (starter prog)))])
    (if (null? pend)
	(filter (lambda (def) (member? (definition-fname def) res)) prog)
	(let* ([all-fnames (gather-applied-fnames (first pend))]
	       [new-fnames (lset-difference eq? all-fnames res)]
	       [new-exprs (map (lambda (fname) (definition-body (assoc fname prog)))
			       new-fnames)])
	  (purge (append new-exprs (rest pend))
		 (delete-duplicates (append new-fnames res)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inlineable? body #;wrt inlineables)
  "is defined procedure with body safe to be inlined?"
  (match body
    [(? static?) #t]
    [(? variable?) #t]
    [((? fname? f) . rands)
     (and (member f inlineables)
	  (every (lambda (x) (inlineable? x inlineables)) rands))]
    [(_ . rands)
     (every (lambda (x) (inlineable? x inlineables)) rands)]))

;(inlineable? '(+ x (f y (g x))) '(f g)) => #t
;(inlineable? '(+ x (f y (h x))) '(f g)) => #f

;; fast and dirty as well/hell -- todo: rewrite using abstract defs [?]
(define (find-all-inlineables prog)
  (let loop ([inlineables '()]
	     [to-check prog])
    (match to-check
      [()
       inlineables]
      [((name args body) . remaining)
       (cond [(member name inlineables)
	      (loop inlineables remaining)]
	     [(inlineable? body inlineables)
	      (loop `(,name . ,inlineables) prog)]
	     [else
	      (loop inlineables remaining)])])))


(define (count-occurences #;of var #;in expr)
  "how many times does var occur in expr"
  (match expr
    [(? static?) 0]    
    [(? variable? v) (if (eq? var v) 1 0)]
    [(_ . rands) (fold-right (lambda (h t)
			       (+ (count-occurences var h) t))
			     0
			     rands)]))

(define (linear-wrt-args? body args)
  "is body linear wrt each of args?"
  (every (lambda (var) (< (count-occurences var body) 2)) args))
;(linear-wrt-args? '(+ (* x x) y) '(x y))


;; fast and dirty as well/hell -- todo: rewrite using abstract defs [?]
(define (beta-reduction body args vals)
  (let ([substitutions (map cons args vals)])
    (let beta ((expr body))
      (match expr
	[(? static?) expr]
	[(? variable? v) (lookup v substitutions)]
	;;; the next one covers ifs as well...
	[(rator . rands) `(,rator . ,(map beta rands))]))))


(define (inline-all inlineables #;inside expr #;in prog)
  (let inline ([expr expr])
    (match expr
      [(? static?) expr]
      [(? variable?) expr]
      [((? fname? f) . rands)
       (let* ([(args body) (lookup f prog)]
	      [vals (map inline rands)])
	 (if (and (member f inlineables)
		  (linear-wrt-args? body args))
	     (beta-reduction (inline body) args vals)		 
	   `(,f . ,(map inline rands))))]
      [(if-or-prim . rands)
       `(,if-or-prim . ,(map inline rands))])))


(define (inline-linear-calls prog)
  (let ([inlineables (find-all-inlineables prog)])
    (map (lambda ((fname varlist body))
	   `(,fname ,varlist ,(inline-all inlineables body prog)))
	 prog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
