(load "../lib/helper.scm")
(load "rand.scm")

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test)))
  ) ; end define

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1)
  ) ; end define

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

(p (estimate-pi 30000))
