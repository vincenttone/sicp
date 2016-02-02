(define (or-gate a1 a2 output)
  (inverter-gate (and-gate
				  (inverter-gate a1)
				  (inverter-gate a2)))
  )

;; (define or-gate-delay (+ (* 3 inverter-gate-delay) and-gate-delay))
