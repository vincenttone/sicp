 (define (make-frame variables values) 
   (if (= (length variables) (length values))
	   (map cons variables values)
	   (error "length mismatch -- MAKE-FRAME" variables values)))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) (car frame))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car frame)))
             (set-car! frame (cons var val)))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
             (add-binding-to-frame! var val frame))
            ((eq? var (car (car frame)))
             (set-car! frame (cons var val)))
            (else (scan (cdr frame)))))
    (scan frame)))