(define (lookup-variable-value-with-callback var val env match-callback end-env-callback)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (match-callback vars vals))
            (else (scan (cdr vars) (cdr vals)))))
	(let ((pre-frame (first-frame env)))
	  (if (eq? env the-empty-environment)
		  (end-env-callback pre-frame)
		  (let* ((frame (first-frame env)) (pre-frame frame))
			(scan (frame-variables frame)
				  (frame-values frame))))))
  (env-loop env))

(define (lookup-variable-value var env)
  (lookup-variable-value-with-callback var
									   false
									   env
									   (lambda (vars vals) (car vals))
									   (lambda (pre-frame) (error "Unbound variable" var))
									   ))

(define (set-variable-value! var val env)
  (lookup-variable-value-with-callback var
									   val
									   env
									   (lambda (vars vals) (set-car! vals val))
									   (lambda (pre-frame) (error "Unbound variable -- SET!" var))
									   ))

(define (define-variable! var val env)
  (lookup-variable-value-with-callback var
									   val
									   env
									   (lambda (vars vals) (set-car! vals val))
									   (lambda (pre-frame) (add-binding-to-frame! var val pre-frame))
									   ))
