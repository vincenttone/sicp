;; (let ((v1 e1) (v2 e2))
;;   ())

;; ((lambda (v1 v2) ()) e1 e2)

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-definition exp)
  (cadr exp)
  )

(define (let-body exp)
  (caddr exp)
  )

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

(define (let-combination exp env)
  (list (make-lambda (let-def-vars (let-definition exp))
					 (let-body exp))
		(let-def-exprs (let-definition exp)))
  )
