(load "../lib/helper.scm")

(define (rand-update x)
	(let ((a 1231) (b 789) (m 465511))
	  (modulo (+ (* a x) b) m)
	  ) ; end let
	) ; end define

(define (make-rand)
  (let ((random-init 12315121))
	(define (rand symbol)
	  (cond ((eq? symbol 'reset)
			 (lambda (x) (set! random-init x))
			 ) ; reset
			((eq? symbol 'generator)
			 (set! random-init (rand-update random-init))
			 random-init) ; enerator
			) ; end cond
	  ) ; end define
	rand) ; end let
) ; end define

(define rand (make-rand))
(repeat 3 (lambda nil (p (rand 'generator))))
(p "reset to 1000")
((rand 'reset) 1000)
(repeat 3 (lambda nil (p (rand 'generator))))

(p "reset to 1000 again")
((rand 'reset) 1000)
(repeat 3 (lambda nil (p (rand 'generator))))
