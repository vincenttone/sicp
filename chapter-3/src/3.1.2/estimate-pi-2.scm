(load "../../lib/helper.scm")
(load "rand.scm")

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init)))
  ) ; end define

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
	(let ((x1 (rand-update x)))
	  (let ((x2 (rand-update x1)))
		(cond ((= trials-remaining 0)
			   (/ trials-passed trials)
			   ) ; c1
			  ((= (gcd x1 x2) 1)
			   (iter (- trials-remaining 1)
					 (+ trials-passed 1)
					 x2) ; end iter
			   ) ; c2
			  (else
			   (iter (- trials-remaining 1)
					 trials-passed
					 x2)) ; c else
		 ) ; end cond
		) ; end let
	  ) ; end let
	) ; end define
  (iter trials 0 initial-x)
  ) ; end define

(p (estimate-pi 30000))
