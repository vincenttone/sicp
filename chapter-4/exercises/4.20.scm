(define (letrec? expr)
  (tagged-list? exp 'letrec))

(define (make-begin seq) (cons 'begin seq))

(define (let-definition exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (letrec-def-vars exp)
  (map (lambda (x) (list (car x) '*unassigned*)) (let-definition exp)))

(define (letrec-set!-body exp)
  (map (lambda (x) (list 'set! (car x) (cadr x))) (let-definition exp)))

(define (letrec->let exp)
  (list 'let (letrec-def-vars exp)
		(make-begin (append (letrec-set!-body exp) (let-body exp)))))

(define s (list 'letrec (list
						 (list 'fact (list 'lambda (list 'n)
										   (list 'if (list '= 'n 1)
												 1
												 (list '* (list 'fact (list '- 'n 1)))))))))

(display (letrec->let s))
(newline)

(define s (list 'letrec (list (list 'even?
								(list 'lambda (list 'n)
									  (list 'if (list '= 'n 0)
											'true
											(list 'odd? (list '- 'n 1)))))
						  (list 'odd?
								(list 'lambda (list 'n)
									  (list 'if (list '= 'n 0)
											'false
											(list 'even? (list '- 'n 1))))))
				(list 'even? 5)))
(display (letrec->let s))
(newline)

;; 只是使用let是不行的，需要使用定义为*unassigned*再从body里赋值的形式
;; (let ((even? *unassigned*) (odd? *unassigned*))
;;   (begin (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
;; 		 (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
;; 		 (even? 5)))
