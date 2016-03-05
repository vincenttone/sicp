(define operation-list 
  (list (cons 'variable? 'lookup-variable-value)
		(cons 'quote? 'text-of-quotation)
		(cons 'assignment? 'eval-assignment)
		(cons 'definition? 'eval-definition)
		(cons 'if? 'eval-if)
		(cons 'lambda? (lambda
						   (exp env) (make-procedure (lambda-parameters exp)
													 (lambda-body exp)
													 env)))
		(cons 'begin? (lambda
						  (eval-sequence (begin-actions exp) env)))
		(cons 'cond? (lambda (eval (cond->if exp) env))))

(define (eval-operation exp env operation-list)
  (if (not (null? operation-list))
	  (if ((caar operation-list) exp)
		  ((cdr (car operation-list) exp env))
		  (eval-operation exp env (cdr operation-list))
		  )))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		(eval-operation exp env operation-list)
		((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp)))
