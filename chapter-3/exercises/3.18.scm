(load "../lib/helper.scm")

(define (recorder)
  (let ((x '()))
	(define (record m)
	  (set! x (cons m x))
	  ) ; define
	(define (recorded? list m)
	  (cond ((null? list) 'no)
			((eq? (car list) m) 'yes)
			(else (recorded? (cdr list) m))
			)
	  ) ; define
	(define (exists? m)
	  (recorded? x m)
	  ) ; define
	(define (dispath s m)
	  (cond ((eq? s 'record) (record m))
			((eq? s 'exists?) (exists? m))
			) ; cond
	  ) ; define
	dispath) ; let
  ) ; define

(define r (recorder))

(define (has-circle? x)
  (if (eq? (r 'exists? x) 'yes)
	  'has-circle
	  (begin (r 'record x)
			 (cond ((not (pair? x)) 'no-circle)
				   (else (has-circle? (cdr x)))
				   ) ; cond
			 ) ; begin
	  ) ; if
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
