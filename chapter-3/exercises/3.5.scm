(load "../lib/helper.scm")

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)
		   (/ trials-passed trials)
		   ) ; c 1
		  ((experiment)
		   (iter (- trials-remaining 1) (+ trials-passed 1))
		   ) ; c 2
		  (else
		   (iter (- trials-remaining 1) trials-passed)
		   ) ; c else
		  ) ; end cond
	) ; end define
  (iter trials 1)
  ) ; end define

(define (random-in-range low high)
  (let ((range (- high low)))
	(+ low (random range))
	) ; end let
  ) ; end define

(define (square x) (* x x))

(define (circle x1 y1 r)
  (define (in-circle? x y)
	(<= (+ 
		 (square (- x x1))
		 (square (- y x1)))
		(square r))
	) ; end define
  in-circle?) ; end define

(define (estimate-integral p? x1 x2 y1 y2 trials)
  (let ((ratio (monte-carlo trials p?))
		(area (* (- x2 x1) (- y2 y1))))
		(let ((circle-area (* ratio area)))
		  circle-area) ; end let
		) ; end let
  ) ; end define
;; 2.457333333333333 not good
(p (let ((x1 2) (x2 8) (y1 4) (y2 10) (cx 5) (cy 7) (r 3) (trials 3000.0))
	 (define in-circle? (circle cx cy r))
	 (let ((circle-area (estimate-integral
						 (lambda nil (in-circle? (random-in-range x1 x2) (random-in-range y1 y2)))
						 x1 x2 y1 y2
						 trials)))
	   (/ circle-area (square r))
	   ) ;end let
	 ) ; end let
   ) ; end p
