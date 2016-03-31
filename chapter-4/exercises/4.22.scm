;; analyze
;; ((let? exp) (analyze-let exp))

(define (analyze-let exp)
  (let ((lproc (analyze (let-combination exp))))
	(lambda (env) (lproc env))))
