(load "../../lib/helper.scm")

(define random-init 12315121)

(define (rand-update x)
  (let ((a 1231) (b 789) (m 465511))
	(modulo (+ (* a x) b) m)
	) ; end let
  ) ; end define

(define rand
  (let ((x random-init))
	(lambda ()
	  (set! x (rand-update x))
	  x) ; end lambda
	) ; end let
  ) ; end define

; (repeat 10 (lambda nil (p (rand))))
