(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b)
				  a
				  (- count 1))
		) ; end if
	))

;; (let x ((v1 e1) (v2 e2))
;;   (x a b))


(define (named-let? expr)
  (and (tagged-list? expr 'let) (symbol? (cadr expr))))

(define (named-let-name expr)
  (cadr expr))

(define (named-let-def expr)
  (caddr expr)
  )

(define (named-let-body expr)
  (cadddr expr)
  )

(define (named-let->func expr)
  (list 'define
		(cons (named-let-name expr)
			  (let-def-vars (named-let-def expr))
			  (named-let-body expr))))

(define (let-def-vars definition)
  (if (null? definition)
	  '()
	  (cons (caar definition) (let-def-var (cdr definition))))
  )

(define (let-def-exprs definition)
  (if (null? definition)
	  '()
	  (cons (cadr (car definition)) (let-def-var (cdr definition))))
  )

(define (let-combination expr env)
  (if (named-let? expr) (sequence->exp (list (named-let->func expr)
											 (cons (named-let-name expr)
												   (let-def-exprs (named-let-def expr)))
											 ))
	  (list (make-lambda (let-def-vars (let-def expr))
						 (let-body expr))
			(let-def-exprs (let-def expr))))
  )
