(define (p value)
  (display value)
  (newline)
  ) ; end define

(define (repeat c fun)
  (if (> c 0)
      (begin
	(fun)
	(repeat (- c 1) fun)
	)
      ) ; end cond
  ) ; end define
