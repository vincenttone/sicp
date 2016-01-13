(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance) ; end begin
      "Isufficient funds") ; end if
  ) ; end define

(display (withdraw 25))
(newline)

(display (withdraw 25))
(newline)

(display (withdraw 60))
(newline)

(display (withdraw 35))
(newline)
