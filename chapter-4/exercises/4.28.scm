(define (foo x) (+ x 1))

(define (bar y) (foo y))

;; 这里如果不使用actual-value，foo产生的就是一个thunk了
;; 这个thunk是一个类似于(list 'thunk (list + x 1) env)的形式
;; 无法调用force-it，这是无法继续执行的
