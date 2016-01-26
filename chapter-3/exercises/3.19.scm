(load "../lib/helper.scm")

(define (has-circle? x)
  (define (check x1 x2)
    (cond ((not (pair? x1)) 'no-circle)
	  ((not (pair? x2)) 'no-circle)
	  ((not (pair? (cdr x2))) 'no-circle)
	  ((eq? (car x1) (cadr (cdr x2)))
	   'has-circle)
	  (else (check (cdr x1) (cddr x2)))
	  ) ; cond
    )
  (check x x)
  ) ; define

(define x (cons 'a 'b))
(define y (cons x x))
(define z (cons y y))
(set-cdr! (cddr z) z)
(p (has-circle? z))

(define x (cons 'a 'b))
(define y (cons x x))
(define z (cons y y))
(p (has-circle? z))
