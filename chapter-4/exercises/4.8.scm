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

(define (named-let? exp)
  (and (tagged-list? exp 'let) (symbol? (cadr exp))))

(define (named-let-name exp)
  (cadr exp))

(define (named-let-def exp)
  (caddr exp)
  )

(define (named-let-body exp)
  (cdddr exp)
  )

(define (named-let->func exp)
  (cons 'define
		(cons (cons (named-let-name exp)
					(let-def-vars (named-let-def exp)))
			  (named-let-body exp))))

(define (let-def-vars definition)
  (if (null? definition)
	  '()
	  (cons (caar definition) (let-def-var (cdr definition))))
  )

(define (let-def-exprs definition)
  (if (null? definition)
	  '()
	  (cons (cadr (car definition)) (let-def-exprs (cdr definition))))
  )

(define (let-combination exp env)
  (if (named-let? exp) (sequence->exp (list (named-let->func exp)
											(cons (named-let-name exp)
												  (let-def-exprs (named-let-def exp)))
											))
	  (cons (make-lambda (let-def-vars (let-definition exp))
						 (let-body exp))
			(let-def-exprs (let-definition exp))))
  )
