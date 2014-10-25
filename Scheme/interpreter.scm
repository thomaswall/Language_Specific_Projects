(define (repl)     
  (display "--> ") 
  (let ((exp (read)))
    (cond ((equal? exp '(exit))      
	   'done)
	  (else  (display (top-eval exp))
		 (newline)
		 (repl))
	  )))


(define (my-load filename)       
  (load-repl (open-input-file filename)))


(define (load-repl port)
  (let ((exp (read port)))
    (cond ((eof-object? exp) 'done)
	  (else (let ((res (top-eval exp)))
		  (display res)
		  (load-repl port)))
	  )))


(define (insert! val L)
  (set-cdr! L (cons (car L) (cdr L)))
  (set-car! L val)
  )


(define (top-eval exp)
  (cond ((not (pair? exp)) (my-eval exp *global-env*))
	((eq? (car exp) 'define)
	 (if
	  (pair? (cadr exp))
	  (begin 
	  	(insert! (list (caadr exp) (my-eval (list 'lambda (cdr (cadr exp)) (cons 'begin (cdr (cdr exp)))) *global-env*)) *global-env*)
	  	(caadr exp))
	  (begin
	  	(insert! (list (cadr exp) (my-eval (caddr exp) *global-env*)) *global-env*)
	  	(cadr exp))
	   ))
	(else (my-eval exp *global-env*))
	))


(define (lookup var env)
  (let ((item (assoc var env)))  ;; assoc returns #f if var not found in env
    (cond ((not item) (display "Error: Undefined Symbol ")
		      (display var) (newline))
	  (else (cadr item))
	  )))

(define (handle-if test then-exp else-exp env)
  (if (my-eval test env)
      (my-eval then-exp env)
      (my-eval else-exp env)))


(define (handle-cond exp env) ;; recurse through all conditionals until hit true or last
	(if (null? (cdr exp)) 
		(handle-block (cdr (car exp)) env) 
		(if (my-eval (car (car exp)) env)
			(handle-block (cdr (car exp)) env)
			(handle-cond (cdr exp) env)
		)
	)
)


(define (handle-let defs body env)
	(let ((bindings (map (lambda (def) (list (car def) (my-eval (cadr def) env))) defs)))
	(handle-block body (append bindings env)))
)

(define (handle-let* defs body env)
	(if (null? (cdr defs))
		(handle-block body (cons(list (car (car defs)) (my-eval (cadr (car defs)) env)) env))
		(handle-let* (cdr defs) body (cons (list (car (car defs)) (my-eval (cadr (car defs)) env)) env))
	)
)


(define (my-eval exp env)
  (cond
   ((symbol? exp) (lookup exp env))
   ((not (pair? exp)) exp)
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'if)
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'cond) (handle-cond (cdr exp) env))
   ((eq? (car exp) 'begin) (handle-block (cdr exp) env))
   ((eq? (car exp) 'lambda)
    (list 'closure exp env))
   ((eq? (car exp) 'let) (handle-let (car (cdr exp)) (cdr (cdr exp)) env))
   ((eq? (car exp) 'let*) (handle-let* (car (cdr exp)) (cdr (cdr exp)) env))
   ((eq? (car exp) 'letrec)
    (handle-letrec (cadr exp) (cddr exp) env)) 
   (else
    (handle-call (map (lambda (sub-exp) (my-eval sub-exp env)) exp)))
   ))


(define (bind formals actuals)
  (cond ((null? formals) '())
	(else (cons (list (car formals) (car actuals))
		    (bind (cdr formals) (cdr actuals))))
	))

(define (handle-block block env)
  (cond ((null? block) (display "Error: Can't have empty block or body"))
	((null? (cdr block)) (my-eval (car block) env))
	(else (my-eval (car block) env)
	      (handle-block (cdr block) env))
	))


;;(

(define (uninit defs orig)
	(if(null? (cdr defs))
		(cons (list (car (car defs)) '*uninitialized*) orig)
		(uninit (cdr defs) (cons (list (car (car defs)) '*uninitialized*) orig))
	)
)

(define (change-env! defs env)
	(if(null? (cdr defs))
		(value_changer! (car (car defs)) (my-eval (cadr (car defs)) env) env)
		(begin 
			(value_changer! (car (car defs)) (my-eval (cadr (car defs)) env) env)
			(change-env! (cdr defs) env)
		)
	)
)

(define (value_changer! key value env)
	(if (eq? (car (car env)) key)
		(set-car! (cdr (car env)) value)
		(value_changer! key value (cdr env))
	)
)

(define (handle-letrec defs body env)
  (let ((new-env (append env (uninit defs '()))))
	(begin
	  (change-env! defs new-env)
	  (handle-block body new-env)
	)
  )
)


(define (handle-call evald-exps)
  (let ((fn (car evald-exps))
	(args (cdr evald-exps)))
   (cond
    ((eq? (car fn) 'closure)
     (let ((formals (cadr (cadr fn)))
	   (body (cddr (cadr fn)))
	   (env (caddr fn)))
       (handle-block body (append (bind formals args) env))))
    ((eq? (car fn) 'primitive-function)
     (apply (cadr fn) args))
    (else (display "Error: Calling non-function"))
    )))


(define (my-apply function args)
	(if (eq? (car function) 'closure)
		(handle-call (cons function args))
		(apply (cadr function) args)
	)
)

(define *global-env*
  (list (list 'car (list 'primitive-function car))
	(list 'cdr (list 'primitive-function cdr))
	(list 'set-car! (list 'primitive-function set-car!))
	(list 'set-cdr! (list 'primitive-function set-cdr!))
	(list 'cons (list 'primitive-function cons))
	(list 'list (list 'primitive-function list))
	(list '+ (list 'primitive-function +))
	(list '- (list 'primitive-function -))
	(list '* (list 'primitive-function *))
	(list '= (list 'primitive-function =))
	(list '< (list 'primitive-function <))
	(list '> (list 'primitive-function >))
	(list '<= (list 'primitive-function  <=))
	(list '>= (list 'primitive-function >=))
	(list 'eq? (list 'primitive-function eq?))
	(list 'pair? (list 'primitive-function pair?))
	(list 'symbol? (list 'primitive-function symbol?))
	(list 'apply (list 'primitive-function my-apply))
	(list 'null? (list 'primitive-function null?))
	(list 'read (list 'primitive-function read))
	(list 'display (list 'primitive-function  display))
	(list 'newline (list 'primitive-function newline))
	(list 'open-input-file (list 'primitive-function open-input-file))
	(list 'close-input-port (list 'primitive-function close-input-port))
	(list 'eof-object? (list 'primitive-function eof-object?))
	(list 'load (list 'primitive-function my-load))
	))