(load "lib.scm")

(define (make-account password balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance) ; end begin
	"Insuffcient funds") ; end if
    ) ; end define
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance) ; end define
  (define count 0)
  (define (incr)
    (set! count (+ count 1))
    count) ; end define
  (define (call-the-cops)
    "Call the cops") ; end define
  (define (dispatch enter-password m)
    (if (eq? enter-password password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT" m))
	      ) ; end cond
	(lambda (x)
	  (begin (incr)
		 (if (> count 7) (call-the-cops)
		     "Incorrect password")
		 ) ; end begin
	  ) ; end lambda
	) ; end if
    ) ; end define
  dispatch) ; end define

(define acc (make-account 'secret-password 100))

(p ((acc 'secret-password 'withdraw) 50))

(repeat 10
	(lambda () (p ((acc 'some-other-password 'withdraw) 60)))
	) ; end repeat

