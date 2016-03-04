;; ((a b +) (x y >) if)
;; ((a b -) (a b +) (x y >) if)

(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (last exp) tag)
	  false))

(define (last exp)
  (if (null? (cdr exp))
	  exp
	  (last (cdr exp)))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) 
  (if (null? (cdddr exp))
	  (cadr exp)
	  (caddr exp)))
(define (if-consequent exp)
  (if (null? (cdddr exp))
	  (car exp)
	  (cadr exp)))
(define (if-alternative exp)
  (if (null? (cdddr exp))
	  'false
	  (car exp)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
