(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)
		     ) ; end lambda
		   ) ; end after delay
      ) ; end let
    ) ; end define
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  ) ; end define

(define (logical-or a1 a2)
  (cond ((= a1 a2) 0)
	(else 1)
	)
  )
