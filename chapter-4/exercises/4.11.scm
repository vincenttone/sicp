(define (make-frame variables values) 
  (if (= (length variables) (length values))
	  (cons '*frame* (map cons variables values))
	  (error "length mismatch -- MAKE-FRAME" variables values)))

(define (frame-variables frame) (map car (cdr frame)))
(define (frame-values frame) (map cdr (cdr frame)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (set-variable-value! var val env)
  (define (assoc var frame)
	(if (null? frame)
		false
		(if (eq? var (caar frame))
			(car frame)
			(assoc var (cdr frame)))))
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let* ((frame (first-frame env))
			   (ret-pair (assoc var (cdr frame))))
          (if ret-pair
			  (set-cdr! ret-pair val)
			  (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (define (assoc var frame)
	(if (null? frame)
		false
		(if (eq? var (caar frame))
			(car frame)
			(assoc var (cdr frame)))))
  (let* ((frame (first-frame env))
		 (ret-pair (assoc var (cdr frame))))
	(if ret-pair
		(set-cdr! ret-pair val)
		(add-binding-to-frame! var val frame))))
