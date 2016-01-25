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

(define (count-pairs x)
  (if (eq? (r 'exists? x) 'yes)
	  0
	  (begin (r 'record x)
			 (if (not (pair? x))
				 0
				 (+ (count-pairs (car x))
					(count-pairs (cdr x))
					1)
				 ) ; if
			 )
	  )
  ) ; define

(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z (cons x y))
(p (count-pairs z))

(define x (cons 'a 'b))
(define y (cons x 'b))
(define z (cons x y))
(p (count-pairs z))

(define x (cons 'a 'b))
(define y (cons x x))
(define z (cons y x))
(p (count-pairs z))

(define x (cons 'a 'b))
(define y (cons x x))
(define z (cons y y))
(p (count-pairs z))

(define x (cons 'a 'b))
(define y (cons x x))
(define z (cons y y))
(set-cdr! (cddr z) z)
(p (count-pairs z))
