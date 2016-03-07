(define true #t)
(define false #f)
(define apply-in-underlying-scheme apply)
(define operation-list 
  (list (cons (lambda (exp) (variable? exp))
			  (lambda (exp env) (lookup-variable-value exp env)))
		(cons (lambda (exp) (quoted? exp))
			  (lambda (exp env) (text-of-quotation exp)))
		(cons (lambda (exp) (assignment? exp))
			  (lambda (exp env) (eval-assignment exp env)))
		(cons (lambda (exp) (definition? exp))
			  (lambda (exp env) (eval-definition exp env)))
		(cons (lambda (exp) (if? exp))
			  (lambda (exp env) (eval-if exp env)))
		(cons (lambda (exp) (and? exp))
			  (lambda (exp env) (eval-and exp env)))
		(cons (lambda (exp) (or? exp))
			  (lambda (exp env) (eval-or exp env)))
		(cons (lambda (exp) (lambda? exp))
			  (lambda (exp env)
				(make-procedure (lambda-parameters exp)
								(lambda-body exp)
								env)))
		(cons (lambda (exp) (let? exp))
			  (lambda (exp env) (eval-let exp env)))
		(cons (lambda (exp) (let*? exp))
			  (lambda (exp env) (eval-let* exp env)))
		(cons (lambda (exp) (unbound? exp))
			  (lambda (exp env) (eval-unbound exp env)))
		(cons (lambda (exp) (while? exp))
			  (lambda (exp env) (eval-while exp env)))
		(cons (lambda (exp) (begin? exp))
			  (lambda (exp env)
				(eval-sequence (begin-actions exp) env)))
		(cons (lambda (exp) (cond? exp))
			  (lambda (exp env) (eval (cond->if exp) env)))))

(define (eval-operation exp env oplist)
  (if (null? oplist)
	  false
	  (if ((car (car oplist)) exp)
		  ((cdr (car oplist)) exp env)
		  (eval-operation exp env (cdr oplist))
		  ))
  )

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((eval-operation exp env operation-list) => (lambda (x) x))
		((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
					   (eval (assignment-value exp) env)
					   env)
  )

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
	  false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (ex-cond? clause) (eq? (cadr clause) '=>))
(define (ex-cond-action clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
			(if (ex-cond? first)
				(make-if (cond-predicate first)
						 (list (ex-cond-action first) (cond-predicate first))
						 (expand-clauses rest))
				(make-if (cond-predicate first)
						 (sequence->exp (cond-actions first))
						 (expand-clauses rest)))))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (eval (and-if (cdr exp) env) env)
  )

(define (and-if exp env)
  (if (null? exp) 'true
	  (make-if (eval (first-exp exp) env)
			   (and-if (rest-exps exp) env)
			   'false)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (eval (or-if (cdr exp) env) env)
  )

(define (or-if exp env)
  (if (null? exp) 'false
	  (make-if (eval (first-exp exp) env)
			   'true
			   (eval-or (rest-exps exp) env))))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-definition exp)
  (cadr exp)
  )

(define (let-body exp)
  (cddr exp)
  )

(define (let-def-vars definition)
  (if (null? definition)
	  '()
	  (cons (caar definition) (let-def-vars (cdr definition))))
  )

(define (let-def-exprs definition)
  (if (null? definition)
	  '()
	  (cons (cadr (car definition)) (let-def-exprs (cdr definition))))
  )

(define (eval-let exp env)
  ;(let-combination exp env))
  (eval (let-combination exp env) env))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*-def exp) (cadr exp))
(define (let*-body exp) (caddr exp))

;; let-combination 无法递归增加lambda(let) body
(define (let-nested-lets exp)
  (define (make-lets defs body)
	(if (null? defs)
		body
		(list 'let (list (car defs)) (make-lets (cdr defs) body)))
	)
  (make-lets (let*-def exp) (let*-body exp))
  )

(define (eval-let* exp env)
  (eval (let-nested-lets exp) env))

;;;; named let
(define (named-let? exp)
  (and (tagged-list? exp 'let) (symbol? (cadr exp))))

(define (named-let-name exp)
  (cadr exp))

(define (named-let-def exp)
  (caddr exp)
  )

(define (named-let-body exp)
  (cdddr exp)
  )

(define (named-let->func exp)
  (cons 'define
		(cons (cons (named-let-name exp)
					(let-def-vars (named-let-def exp)))
			  (named-let-body exp))))

;;;; let-combination
(define (let-combination exp env)
  (if (named-let? exp) (sequence->exp (list (named-let->func exp)
											(cons (named-let-name exp)
												  (let-def-exprs (named-let-def exp)))
											))
	  (cons (make-lambda (let-def-vars (let-definition exp))
						 (let-body exp))
			(let-def-exprs (let-definition exp))))
  )

;;; while
(define (eval-while exp env)
  (while->combination exp))
  ;(eval (while->combination exp) env))

(define (while? exp) (tagged-list? exp 'while))
(define (while-condition exp) (cadr exp))
(define (while-body exp) (caddr exp))
(define (while->combination exp)
  (sequence->exp
   (list (list 'define
			   (list 'while-iter)
			   (make-if (while-condition exp)
						(sequence->exp (list (while-body exp)
											 (list 'while-iter)))
						'true))
		 (list 'while-iter))))

;;; end while def

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;;; updated frame
;; (define (make-frame variables values) 
;;   (if (= (length variables) (length values))
;; 	  (cons '*frame* (map cons variables values))
;; 	  (error "length mismatch -- MAKE-FRAME" variables values)))

;; (define (frame-variables frame) (map car (cdr frame)))
;; (define (frame-values frame) (map cdr (cdr frame)))

;; (define (add-binding-to-frame! var val frame)
;;   (set-cdr! frame (cons (cons var val) (cdr frame))))
;;;;

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; (define (lookup-variable-value var env)
;;   (define (env-loop env)
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (env-loop (enclosing-environment env)))
;;             ((eq? var (car vars))
;;              (car vals))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable" var)
;;         (let ((frame (first-frame env)))
;;           (scan (frame-variables frame)
;;                 (frame-values frame)))))
;;   (env-loop env))

;; (define (set-variable-value! var val env)
;;   (define (env-loop env)
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (env-loop (enclosing-environment env)))
;;             ((eq? var (car vars))
;;              (set-car! vals val))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable -- SET!" var)
;;         (let ((frame (first-frame env)))
;;           (scan (frame-variables frame)
;;                 (frame-values frame)))))
;;   (env-loop env))

;; (define (define-variable! var val env)
;;   (let ((frame (first-frame env)))
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (add-binding-to-frame! var val frame))
;;             ((eq? var (car vars))
;;              (set-car! vals val))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (scan (frame-variables frame)
;;           (frame-values frame))))

;;;; updated frame
;; (define (set-variable-value! var val env)
;;   (define (assoc var frame)
;; 	(if (null? frame)
;; 		false
;; 		(if (eq? var (caar frame))
;; 			(car frame)
;; 			(assoc var (cdr frame)))))
;;   (define (env-loop env)
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable -- SET!" var)
;;         (let* ((frame (first-frame env))
;; 			   (ret-pair (assoc var (cdr frame))))
;;           (if ret-pair
;; 			  (set-cdr! ret-pair val)
;; 			  (env-loop (enclosing-environment env))))))
;;   (env-loop env))

;; (define (define-variable! var val env)
;;   (define (assoc var frame)
;; 	(if (null? frame)
;; 		false
;; 		(if (eq? var (caar frame))
;; 			(car frame)
;; 			(assoc var (cdr frame)))))
;;   (let* ((frame (first-frame env))
;; 		 (ret-pair (assoc var (cdr frame))))
;; 	(if ret-pair
;; 		(set-cdr! ret-pair val)
;; 		(add-binding-to-frame! var val frame))))
;;;;

;;;; 4.12
(define (lookup-variable-value-with-callback var val env match-callback end-env-callback)
  (let ((pre-frame the-empty-environment))
	(define (env-loop env)
	  (define (scan vars vals)
		(cond ((null? vars)
			   (env-loop (enclosing-environment env)))
			  ((eq? var (car vars))
			   (match-callback vars vals))
			  (else (scan (cdr vars) (cdr vals)))))
	  (if (eq? env the-empty-environment)
		  (end-env-callback pre-frame)
		  (let ((frame (first-frame env)))
			(set! pre-frame frame)
			(scan (frame-variables frame)
				  (frame-values frame)))))
  (env-loop env)))

(define (lookup-variable-value var env)
  (lookup-variable-value-with-callback var
									   false
									   env
									   (lambda (vars vals) (car vals))
									   (lambda (pre-frame) (error "Unbound variable" var))
									   ))

(define (set-variable-value! var val env)
  (lookup-variable-value-with-callback var
									   val
									   env
									   (lambda (vars vals) (set-car! vals val))
									   (lambda (pre-frame) (error "Unbound variable -- SET!" var))
									   ))

(define (define-variable! var val env)
  (lookup-variable-value-with-callback var
									   val
									   env
									   (lambda (vars vals) (set-car! vals val))
									   (lambda (pre-frame) (add-binding-to-frame! var val pre-frame))
									   ))
;;; end 4.12

;;; begin 4.13
(define (unbound? expr) (tagged-list? expr 'unbound))
(define (unbind-variable expr env) (make-unbound! (cadr expr) env))
(define (make-unbound! var env)
  (define (env-loop env)
    (define (scan vars vals pre-vars pre-vals)
      (cond ((null? vars)
             (error "Unbound variable -- UNBOUND!" var))
            ((eq? var (car vars))
			 (set-car! env (cons (append pre-vars (cdr vars)) (append pre-vals (cdr vals)))))
            (else (scan (cdr vars)
						(cdr vals)
						(cons (car vars) pre-vars)
						(cons (car vals) pre-vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- UNBOUND!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)
				'()
				'()))))
  (env-loop env))
(define (eval-unbound expr env)
  (make-unbound! (cadr expr) env))
;;; end 4.13

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(driver-loop)
