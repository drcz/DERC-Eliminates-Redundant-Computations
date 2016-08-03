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
  ;;; todo
  prog)


(define (inline-linear-calls prog)
  ;;; todo
  prog)

