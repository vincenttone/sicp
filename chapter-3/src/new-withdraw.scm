;; 因为balance是一个全局变量，调整为new-withdraw

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance) ; end begin
	  "Insufficient funds") ; end if
      ) ; end lambda
    ) ; end let
  ) ; end new-withdraw

(display (new-withdraw 25))
(newline)

(display (new-withdraw 25))
(newline)
