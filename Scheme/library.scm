(define (append L1 L2)
  (cond ((null? L1) L2)
        (else (cons (car L1)  (append (cdr L1) L2)))
        )
)

(define (map f L)
   (cond ((null? L) '())
         (else (cons (f (car L)) (map f (cdr L))))
   )
)

(define (assoc key key-values)
	(if (eq? (car (car key-values)) key)
		(car key-values)
		(assoc key (cdr key-values))
	)
)

(define (cadr L)
	(car (cdr L)))

(define (caddr L)
	(car (cdr (cdr L))))

(define (caadr L)
	(car (car (cdr L))))

(define (cddr L)
	(cdr (cdr L)))

(define (cadddr L)
	(car (cdr (cdr (cdr L)))))