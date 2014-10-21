(define (my-append L1 L2)
  (cond ((null? L1) L2)
        (else (cons (car L1)  (my-append (cdr L1) L2)))
        )
)

(define (my-map f L)
   (cond ((null? L) '())
         (else (cons (f (car L)) (my-map f (cdr L))))
   )
)

(define (my-assoc key key-values)
	(if (eq? (car (car key-values)) key)
		(cadr (car key-values))
		(my-assoc key (cdr key-values))
	)
)

(define (my-apply function args)

)