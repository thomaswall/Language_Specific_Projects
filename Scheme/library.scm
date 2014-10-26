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

(define (equal? L1 L2)
	(if (or (not (pair? L1)) (not (pair? L2)))
		(eq? L1 L2)
		(cond
				  ((and (null? L1) (null? L2)) #t)
				  ((and (null? L1) (not (null? L2))) #f)
				  ((and (null? L2) (not (null? L1))) #f)
				  ((or (not (pair? (car L1))) (not (pair? (car L2))))
						(if (eq? (car L1) (car L2))
				 		     (equal? (cdr L1) (cdr L2))
				 		     #f))
				  (else (equal? (car L1) (car L2)) (equal? (cdr L1) (cdr L2)))))
)

(define (and x y)
  (if x y #f))

(define (or x y)
  (if x #t y))

(define (not x)
	(if x #f #t))

(define (cadr L)
	(car (cdr L)))

(define (caddr L)
	(car (cdr (cdr L))))

(define (caadr L)
	(car (car (cdr L))))

(define (cddr L)
	(cdr (cdr L)))

(define (cdddr L)
	(cdr (cdr (cdr L))))

(define (cadddr L)
	(car (cdr (cdr (cdr L)))))