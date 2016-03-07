;; 因为使用系统的map的时候，map调用的是系统的函数定义
;; 我们的解释器定义函数时是一个等待解释器解释的compound-procedure
;; 而不一定是primitive-procedure
;; 一个(define (x a b) (cons a b))  (map x (list 'a 'b 'c) (list 1 2 3))
;; 产生的是 (map (list 'procedure (a b) ((cons a b)) env) (list 'a 'b 'c) (list 1 2 3))

(define (x a b) (cons a b))

;; define的处理过程如下
((definition? exp) (eval-definition exp env))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body
;; 产生一个lambda
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
;; eval中lambda会做如下处理
((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
;; 于是产生一个procedure list，此过程需要交给apply执行
;; 也就是产生了一个 (list 'procedure (a b) ((cons a b)) env)
(map x (list 'a 'b 'c) (list 1 2 3))
;; 产生的是
(map (list 'procedure (a b) ((cons a b)) env) (list 'a 'b 'c) (list 1 2 3))

;; map需要一个过程，而不是一个list
;; 所以无法执行
