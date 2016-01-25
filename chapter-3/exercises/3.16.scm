(load "../lib/helper.scm")

(define (count-pairs x)
  (if (not (pair? x))
	  0
	  (+ (count-pairs (car x))
		 (count-pairs (cdr x))
		 1)
	  ) ; if
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
