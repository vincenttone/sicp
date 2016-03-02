;; (let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
;;   (* x z))

;; (let ((x 3))
;;   (let ((y (+ x 2)))
;; 	  (let ((z (+ x y 5)))
;; 	    (* x z))))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*-def exp) (cadr exp))
(define (let*-body exp) (caddr exp))

;; let-combination 无法递归增加lambda(let) body
(define (let-nested-lets exp)
  (define (make-lets defs body)
	(if (null? defs) body
		(list 'let (list (car defs)) (make-lets (cdr defs))))
	)
  (make-lets (let*-def exp) (let*-body exp))
  )
