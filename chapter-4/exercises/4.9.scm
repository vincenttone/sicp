;;;; not sure
;; eval 
((while? expr) (eval-while expr env))

(define (eval-while exp env)
  (eval (while->combination exp) env))

;; while
;; (while (< a 10) (...))
(define (while? expr) (tagged-list? expr 'while))
(define (while-condition expr) (cadr expr))
(define (while-body expr) (caddr expr))
(define (while->combination expr)
  (sequence->exp
   (list (list 'define
			   (list 'while-iter)
			   (make-if (while-condition expr)
						(sequence->exp (list (while-body expr)
											 (list 'while-iter)))
						'true))
		 (list 'while-iter))))
