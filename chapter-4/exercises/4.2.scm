;;;;
;; a) application判断条件是pair? 所以复合数据都会通过application?
;;;;
(define (eval exp env)
  (cond ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
		;; case (define (application? exp) (pair? exp))
		;; all of pair exp will passed by application?
		((self-evaluating? exp) exp)
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
		;; 所谓的重新安排cond字句的位置不是这个cond，而是整个子句。。。
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;;;
;; b)
;;;;

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
