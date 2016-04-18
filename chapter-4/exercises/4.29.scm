;; 1)
(define (foo x) (if (= x 0) x (+ x (foo (- x 1)))))

(define (bar y)
  (define (iter callback)
	(cond ((= t 0) 0)
		  (else (callback) (iter (- t 1)))))
  (iter (bar 100000)))

;; 2)
;; 有惰性求值返回1
;; 无惰性求值返回2
;; 取决于求值次数
