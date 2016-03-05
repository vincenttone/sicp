;;;;
;; a)
;;;;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (eval (and-if (cdr exp) env) env)
  )

(define (and-if exp env)
  (if (null? exp) 'true
	  (make-if (eval (first-exp exp) env)
			   (and-if (rest-exps exp) env)
			   'false)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (eval (or-if (cdr exp) env) env)
  )

(define (or-if exp env)
  (if (null? exp) 'false
	  (make-if (eval (first-exp exp) env)
			   'true
			   (eval-or (rest-exps exp) env))))


;;;;
;; b)
;;;;

(cond ...
 ((and? exp) (expand-and-clasuses (cdr exp)) env)
 ((or? exp) (expand-or-clasuses (cdr exp)) env)
 ...
 )

(define (expand-and-clauses clauses)
  (if (null? clasuses)
	  'true
	  (make-if (car clasuses)
			   (expand-and-clauses (cdr clasuses))
			   'false
			   )
	  ))

(define (expand-or-clauses clauses)
  (if (null? clasuses)
	  'false
	  (make-if (car clasuses)
			   'true
			   (expand-and-clauses (cdr clasuses)))
	  ))
