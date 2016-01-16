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
  (define (dispatch enter-password m)
    (if (eq? enter-password password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      ((eq? m 'current) balance)
	      ((eq? m 'joint) 'ok)
	      (else (error "Unknown request -- MAKE-ACCOUNT" m))
	      ) ; end cond
	(lambda (x) "Incorrect password")) ; end if
    ) ; end define
  dispatch) ; end define

(define (make-joint account original-password new-password)
  (define (dispatch enter-password m)
    (if (eq? enter-password new-password)
	(account original-password m)
	(lambda (x) "Incorrect password")) ; end if
    ) ; end define
  (define (wrong-original-password)
    (display "Incorrect joint password")
    (newline)
    )
  (cond ((eq? (account original-password 'joint) 'ok)
	 dispatch)
	(else (wrong-original-password)
	      (lambda (x y) (lambda (a) (wrong-original-password) 'failed)
		      ) ; end lambda
	      ) ; end else
      ) ; end cond
  ) ; end define

(define peter-acc (make-account 'secret-password 100))

(p ((peter-acc 'secret-password 'withdraw) 50))
(p ((peter-acc 'some-other-password 'withdraw) 60))
(p ((peter-acc 'secret-password 'deposit) 40))
(p (peter-acc 'secret-password 'current))

(define paul-acc
  (make-joint peter-acc 'secret-password 'rosebud)
  ) ; end define

(p ((paul-acc 'rosebud 'withdraw) 50))
(p ((paul-acc 'not-rosebud 'withdraw) 50))
