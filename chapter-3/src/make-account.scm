;; 面向对象形式的存取款

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance) ; end begin
	"Insuffcient funds") ; end if
    ) ; end define
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance) ; end define
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT" m))
	  ) ; end cond
    ) ; end define
  dispatch) ; end define

(define acc (make-account 100))

(define (p value)
  (display value)
  (newline)
  ) ; end define

(p ((acc 'withdraw) 50))
(p ((acc 'withdraw) 60))
(p ((acc 'deposit) 40))
(p ((acc 'withdraw) 60))
