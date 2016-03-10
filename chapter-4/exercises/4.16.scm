(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
			 (if (eq? '*unassigned* (car vals))
				 (error "Unassigend variable" var)
				 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (scan-out-defines procs)
  (let ((inits '()) (body '()))
	(define (scan-out-let-struct procs)
	  (cond ((not (null? procs))
			 (let ((first-proc (car procs)))
			   (cond ((pair? first-proc)
					  (cond ((eq? 'define (car first-proc))
							 (if (pair? (cadr first-proc)) ;; lambda
								 (begin (set! inits (cons (list (car (cadr first-proc)) '*unassigned*) inits))
										(set! body 
											  (cons (list 'set!
														  (car (cadr first-proc))
														  (make-lambda (cdr (cadr first-proc)) (cddr first-proc))) body)))
								 (begin (set! inits (cons (list (cadr first-proc) '*unassigned*) inits))
										(set! body (cons (list 'set! (cadr first-proc) (caddr first-proc)) body)))))
							(else (set! body (cons first-proc body)))))

					 (else (set! body (cons first-proc body))))
			   )
			 (scan-out-let-struct (cdr procs))))
	  (set! inits (reverse inits))
	  (set! body (reverse body))
	  )
	(scan-out-let-struct procs)
	(cons 'let (cons inits body))
	))

(display (scan-out-defines (list (list 'define (list 'm 'a 'b) (list '+ 'a 'b)) (list 'define 'n 100) (list 'define 'o 12312) 10000)))
(newline)

;; make-procedure 里面调用可以在分析语法的时候把scan-out-defines的结果保存起来(如果是在define的方法里面)，多次调用的时候不需要重复转换
;; 最上层的 (define (x ...) ...)会转化为lambda，lambda会调用make-procedure产生一个procedure，这个procedure会放在变量表里
;; 如果放在make-procedure，apply的时候就是scan-out-defines后的procedure
;; 如果放在procedure-body，则每次apply x方法的时候都需要调用scan-out-defines做转换
