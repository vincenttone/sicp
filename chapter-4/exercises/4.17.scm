;; a)
;; 因为let表达式会被转换成一个lambda表达式，lambda表达式是一个procedure，使用compound-procedure?检查发现是一个procedure之后，会调用extend-environment产生一个新环境，会调用make-frame产生一个新框架

((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))

((compound-procedure? procedure)
         (eval-sequence
		  (procedure-body procedure)
		  (extend-environment  ;; 新环境
		   (procedure-parameters procedure)
		   arguments
		   (procedure-environment procedure))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)  ;; 新的frame
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; b)
;; 经过转换后的代码作为let表达式，会产生一个新的环境，上层环境不会受影响

;; c)
;; 保证调用在定义之后，可以重新调整定义和调用顺序，以防出现调用到未定义的方法，出现同时定义的效果。
;; 参考了 https://wqzhang.wordpress.com/2009/11/13/sicp-exercise-4-17/
;; 下面直接拿来了代码，工作就是拿到所有的define并前移，执行体置后
(define (rearrange-defines body)
  (let ((defs '()) (others '()))
    (let scan-iter ((b body))
      (cond ((null? b)
             '())
            ((definition? (car b))
             (set! defs (append defs (list (car b))))) 
            (else 
             (set! others (append others (list (car b))))))
      (if (not (null? b))
          (scan-iter (cdr b))))
    (if (null? defs)
        body
        (append defs others))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (rearrange-defines body) env))
